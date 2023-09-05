use crate::{Debugger, Tracee};

use nix::libc;
use nix::sys::socket::{self, SockaddrLike};
use nix::sys::{signal, stat};

use std::ffi::{c_int, CString};
use std::fmt;
use std::mem::size_of;
use std::os::fd::AsRawFd;
use std::ptr;

#[cfg(target_arch = "x86_64")]
pub type Sysno = syscalls::x86_64::Sysno;

#[cfg(target_arch = "x86")]
pub type Sysno = syscalls::x86::Sysno;

#[cfg(target_arch = "aarch64")]
pub type Sysno = syscalls::aarch64::Sysno;

#[cfg(target_arch = "arm")]
pub type Sysno = syscalls::arm::Sysno;

const ETH_ALL: c_int = libc::ETH_P_ALL.to_be();

macro_rules! print_delimited {
    [$str:expr, $x:expr, $($xs:expr),+] => {{
        $str += &$x;
        $str += ", ";
        print_delimited![$str, $($xs),+];
    }};

    [$str:expr, $x:expr] => {{
        $str += &$x;
    }};

    [] => {{}};
}

macro_rules! format_flags {
    ($flags:expr => $ty:ty) => {
        match <$ty>::from_bits($flags as _) {
            Some(flag) => format!("{flag:?}"),
            None => format!("(unknown)"),
        }
    };
}

#[repr(transparent)]
struct IoVec(libc::iovec);

impl fmt::Debug for IoVec {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let base = format_ptr(self.0.iov_base as u64);

        f.write_fmt(format_args!("{{base: {base}, ",))?;
        f.write_fmt(format_args!("len: {:?}}}", self.0.iov_len))
    }
}

#[repr(transparent)]
struct Fd(c_int);

impl fmt::Debug for Fd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&format_fd(self.0 as u64))
    }
}

#[repr(transparent)]
struct PollFd(libc::pollfd);

impl fmt::Debug for PollFd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{{fd: {:?}, ", Fd(self.0.fd)))?;

        match nix::poll::PollFlags::from_bits(self.0.events) {
            Some(events) => f.write_fmt(format_args!("events: {events:?}}}")),
            None => f.write_fmt(format_args!("events: {}}}", self.0.events)),
        }
    }
}

fn format_ptr(addr: u64) -> String {
    if addr == 0 {
        "NULL".to_string()
    } else {
        format!("{addr:#x}")
    }
}

fn format_fd(fd: u64) -> String {
    match fd as c_int {
        0 => "stdin".to_string(),
        1 => "stdout".to_string(),
        2 => "stderr".to_string(),
        n => n.to_string(),
    }
}

fn format_fdset(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<nix::sys::select::FdSet>()) {
        Ok(data) => {
            let set = unsafe { ptr::read(data.as_ptr() as *const nix::sys::select::FdSet) };
            let set: Vec<_> = set.fds(None).map(|fd| Fd(fd.as_raw_fd())).collect();

            format!("{set:?}")
        }
        Err(..) => format!("???"),
    }
}

// Try to read 20 bytes.
//
// TODO: guess whether the bytes could be a utf-8 sequence
fn format_bytes_u8(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = std::cmp::min(len as usize, 20);
    match session.read_process_memory(addr as usize, bytes_to_read) {
        Ok(data) => {
            let mut data = format!("{data:x?}");

            if len > 60 {
                data.pop();
                data += ", ..]";
            }

            data
        }
        Err(..) => format!("???"),
    }
}

/// Read first 20 elements of an array of type `T`.
fn format_array<T: std::fmt::Debug>(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let width = size_of::<T>();
    let end_of_array = addr + len * width as u64;
    let mut items = Vec::new();
    for addr in (addr..end_of_array).step_by(width).take(20) {
        if let Ok(data) = session.read_process_memory(addr as usize, width) {
            let item = unsafe { ptr::read(data.as_ptr() as *const T) };
            items.push(item);
        }
    }

    format!("{items:?}")
}

/// Try to read a string with a known length and print the first 60 characters.
fn format_str(session: &mut Debugger, addr: u64, len: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = std::cmp::min(len as usize, 60 * size_of::<char>());
    match session.read_process_memory(addr as usize, bytes_to_read) {
        Ok(data) => {
            let data = String::from_utf8_lossy(&data).into_owned();
            let mut data = data.escape_default().to_string();

            if data.len() > 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("???"),
    }
}

/// Try to read a string with a null terminator and print the first 60 characters.
fn format_c_str(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let bytes_to_read = 60 * size_of::<char>();
    match session.read_process_memory(addr as usize, bytes_to_read) {
        Ok(mut data) => {
            // add a null terminator if one wasn't found in the first 40 bytes
            match data.iter().position(|&b| b == b'\0') {
                Some(terminator) => data.truncate(terminator + 1),
                None => {
                    data.pop();
                    data.push(b'\0')
                }
            }

            let data = match CString::from_vec_with_nul(data.clone()) {
                Ok(data) => data.to_string_lossy().into_owned(),
                Err(..) => return format!("???"),
            };

            let mut data = data.escape_default().to_string();
            if data.len() > 60 {
                data.truncate(57);
                data += "...";
            }

            format!("\"{data}\"")
        }
        Err(..) => format!("???"),
    }
}

// FIXME: i don't think this is being interpreted correctly
fn format_sigset(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<signal::SigSet>()) {
        Ok(data) => {
            let set = unsafe { ptr::read(data.as_ptr() as *const signal::SigSet) };

            if set == signal::SigSet::all() {
                return format!("~[]");
            }

            let set: Vec<signal::Signal> = set.iter().collect();

            // if all 31 signals are set, it must be an empty set mask
            match set.len() {
                31 => format!("~[]"),
                _ => format!("{set:?}"),
            }
        }
        Err(..) => format!("???"),
    }
}

fn format_sigaction(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<signal::SigAction>()) {
        Ok(data) => {
            let action = unsafe { ptr::read(data.as_ptr() as *const signal::SigAction) };
            let handler = action.handler();
            let flags = action.flags();
            let mask = action.mask();
            let mask: Vec<signal::Signal> = mask.iter().collect();

            format!("{{sa_handler: {handler:?}, sa_mask: {mask:?}, sa_flags: {flags:?}}}")
        }
        Err(..) => format!("???"),
    }
}

fn format_futex_op(op: u64) -> &'static str {
    let op = op as c_int;

    if op & libc::FUTEX_PRIVATE_FLAG == libc::FUTEX_PRIVATE_FLAG {
        match op - libc::FUTEX_PRIVATE_FLAG {
            libc::FUTEX_WAIT => "FUTEX_WAIT_PRIVATE",
            libc::FUTEX_WAKE => "FUTEX_WAKE_PRIVATE",
            libc::FUTEX_FD => "FUTEX_FD_PRIVATE",
            libc::FUTEX_REQUEUE => "FUTEX_REQUEUE_PRIVATE",
            libc::FUTEX_CMP_REQUEUE => "FUTEX_CMP_REQUEUE_PRIVATE",
            _ => "(unknown)",
        }
    } else {
        match op {
            libc::FUTEX_WAIT => "FUTEX_WAIT",
            libc::FUTEX_WAKE => "FUTEX_WAKE",
            libc::FUTEX_FD => "FUTEX_FD",
            libc::FUTEX_REQUEUE => "FUTEX_REQUEUE",
            libc::FUTEX_CMP_REQUEUE => "FUTEX_CMP_REQUEUE",
            _ => "(unknown)",
        }
    }
}

fn format_stat(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<stat::FileStat>()) {
        Ok(data) => {
            let stats = unsafe { ptr::read(data.as_ptr() as *const stat::FileStat) };
            let mode = stats.st_mode;
            let size = stats.st_size;

            format!("{{st_mode={mode:?}, st_size={size}, ...}}")
        }
        Err(..) => format!("???"),
    }
}

fn format_ioctl(request: u64) -> &'static str {
    // don't ask me why we only take the first 15 bits, I don't know
    //
    // FIXME: figure out what's happening here
    let request = request as u32 & 0b111111111111111;

    if cfg!(target_arch = "x86_64") {
        let search = super::ioctl::ARCH_CODES.iter().find(|(_, code)| code == &request);
        if let Some((name, _)) = search {
            return name;
        }
    }

    let search = super::ioctl::GENERIC_CODES.iter().find(|(_, code)| code == &request);
    if let Some((name, _)) = search {
        return name;
    }

    "???"
}

fn format_timespec(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<nix::sys::time::TimeSpec>()) {
        Ok(data) => {
            let time = unsafe { ptr::read(data.as_ptr() as *const nix::sys::time::TimeSpec) };
            let duration = std::time::Duration::from(time);

            format!("{duration:#?}")
        }
        Err(..) => format!("???"),
    }
}

fn format_timerval(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    match session.read_process_memory(addr as usize, size_of::<nix::sys::time::TimeVal>()) {
        Ok(data) => {
            let time = unsafe { ptr::read(data.as_ptr() as *const nix::sys::time::TimeVal) };
            format!("{time}")
        }
        Err(..) => format!("???"),
    }
}

fn format_itimerval(session: &mut Debugger, addr: u64) -> String {
    if addr == 0 {
        return "NULL".to_string();
    }

    let interval = format_timerval(session, addr);
    let next = format_timerval(session, addr + size_of::<nix::sys::time::TimeVal>() as u64);

    format!("{{interval: {interval}, next: {next}}}")
}

fn format_sockaddr(session: &mut Debugger, addr: u64, socketlen: Option<u32>) -> String {
    let addr = addr as usize;

    if addr == 0 {
        return "NULL".to_string();
    }

    // read the first field of any sockaddr struct, it includes what family of addresses we
    // are working with
    let family = match session.read_process_memory(addr, size_of::<libc::sa_family_t>()) {
        Ok(data) => {
            let raw = libc::sa_family_t::from_le_bytes(data.try_into().unwrap()) as i32;

            if raw == libc::AF_UNSPEC {
                return format!("(opaque)");
            }

            match socket::AddressFamily::from_i32(raw) {
                Some(family) => family,
                None => return format!("(unknown address family)"),
            }
        }
        Err(..) => return format!("???"),
    };

    match family {
        // struct sockaddr_in
        socket::AddressFamily::Inet => {
            let read = session.read_process_memory(addr, size_of::<socket::sockaddr_in>());
            let sock_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::sockaddr_in) },
                Err(..) => return format!("???"),
            };

            let addr = std::net::Ipv4Addr::from(sock_addr.sin_addr.s_addr);
            let port = sock_addr.sin_port;

            format!("{{addr: {addr}, port: {port}}}")
        }
        // struct sockaddr_in6
        socket::AddressFamily::Inet6 => {
            let read = session.read_process_memory(addr, size_of::<socket::sockaddr_in6>());
            let sock_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::sockaddr_in6) },
                Err(..) => return format!("???"),
            };

            let addr = std::net::Ipv6Addr::from(sock_addr.sin6_addr.s6_addr);
            let port = sock_addr.sin6_port;

            format!("{{addr: {addr}, port: {port}}}")
        }
        // struct sockaddr_un
        socket::AddressFamily::Unix => {
            let read = session.read_process_memory(addr, size_of::<socket::sockaddr>());
            let sock_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::sockaddr) },
                Err(..) => return format!("???"),
            };

            // SAFETY: since we pass the length, it will be validated
            let unix_addr = unsafe {
                match socket::UnixAddr::from_raw(&sock_addr, socketlen) {
                    Some(addr) => addr,
                    None => return format!("???"),
                }
            };

            match unix_addr.path() {
                Some(path) => format!("{{path: {path:#?}}}"),
                None => format!("???"),
            }
        }
        // struct sockaddr_nl
        socket::AddressFamily::Netlink => {
            let read = session.read_process_memory(addr, size_of::<socket::NetlinkAddr>());
            let netlink_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::NetlinkAddr) },
                Err(..) => return format!("???"),
            };

            let pid = netlink_addr.pid();
            let groups = netlink_addr.groups();

            format!("{{pid: {pid}, groups: {groups}}}")
        }
        // struct sockaddr_alg
        socket::AddressFamily::Alg => {
            let read = session.read_process_memory(addr, size_of::<socket::AlgAddr>());
            let alg_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::AlgAddr) },
                Err(..) => return format!("???"),
            };

            let tipe = alg_addr.alg_type().to_string_lossy();
            let name = alg_addr.alg_name().to_string_lossy();

            format!("{{type: {tipe}, name: {name}}}")
        }
        // struct sockaddr_ll
        socket::AddressFamily::Packet => {
            let read = session.read_process_memory(addr, size_of::<socket::LinkAddr>());
            let link_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::LinkAddr) },
                Err(..) => return format!("???"),
            };

            let protocol = link_addr.protocol();
            let iface = link_addr.ifindex();

            match link_addr.addr() {
                Some(mac) => {
                    let mac = format!(
                        "{:<02X}:{:<02X}:{:<02X}:{:<02X}:{:<02X}:{:<02X}",
                        mac[0], mac[1], mac[2], mac[3], mac[4], mac[5]
                    );

                    format!("{{protocol: {protocol}, iface: {iface}, mac: {mac}}}")
                }
                None => format!("{{protocol: {protocol}, iface: {iface}}}"),
            }
        }
        // struct sockaddr_vm
        socket::AddressFamily::Vsock => {
            let read = session.read_process_memory(addr, size_of::<socket::VsockAddr>());
            let vsock_addr = match read {
                Ok(data) => unsafe { ptr::read(data.as_ptr() as *const socket::VsockAddr) },
                Err(..) => return format!("???"),
            };

            let cid = vsock_addr.cid();
            let port = vsock_addr.port();

            format!("{{cid: {cid}, port: {port}}}")
        }
        _ => format!("(unknown address family)"),
    }
}

/// `sock_addr` is a *const sockaddr and `len_addr` is a *const u32.
fn format_sockaddr_using_len(session: &mut Debugger, sock_addr: u64, len_addr: u64) -> String {
    if len_addr == 0 {
        return format_sockaddr(session, sock_addr, None);
    }

    match session.read_process_memory(len_addr as usize, size_of::<u32>()) {
        Ok(data) => {
            let len = unsafe { ptr::read(data.as_ptr() as *const u32) };
            format_sockaddr(session, sock_addr, Some(len))
        }
        Err(..) => "???".to_string(),
    }
}

fn format_sock_protocol(protocol: u64) -> &'static str {
    match protocol as c_int {
        libc::IPPROTO_TCP => "Tcp",
        libc::IPPROTO_UDP => "Udp",
        libc::IPPROTO_RAW => "Ip",
        libc::NETLINK_ROUTE => "NetlinkRoute",
        libc::NETLINK_USERSOCK => "NetlinkUsersock",
        libc::NETLINK_SOCK_DIAG => "NetlinkSockDiag",
        libc::NETLINK_SELINUX => "NetlinkSELINUX",
        libc::NETLINK_ISCSI => "NetlinkISCSI",
        libc::NETLINK_AUDIT => "NetlinkAudit",
        libc::NETLINK_FIB_LOOKUP => "NetlinkFIBLookup",
        libc::NETLINK_NETFILTER => "NetlinkNetfilter",
        libc::NETLINK_SCSITRANSPORT => "NetlinkSCSITransport",
        libc::NETLINK_RDMA => "NetlinkRDMA",
        libc::NETLINK_IP6_FW => "NetlinkIpv6Firewall",
        libc::NETLINK_DNRTMSG => "NetlinkDECNetroutingMsg",
        libc::NETLINK_KOBJECT_UEVENT => "NetlinkKObjectUEvent",
        libc::NETLINK_CRYPTO => "NetlinkCrypto",
        ETH_ALL => "EthAll",
        _ => "(unknown)",
    }
}

fn format_msghdr(session: &mut Debugger, addr: u64) -> String {
    let msghdr = match session.read_process_memory(addr as usize, size_of::<libc::msghdr>()) {
        Ok(data) => unsafe { ptr::read(data.as_ptr() as *const libc::msghdr) },
        Err(..) => return format!("???"),
    };

    let name = format_sockaddr(session, msghdr.msg_name as u64, Some(msghdr.msg_namelen));
    let name_len = msghdr.msg_namelen;

    let msg_iov = format_array::<IoVec>(session, msghdr.msg_iov as u64, msghdr.msg_iovlen as u64);
    let msg_iov_len = msghdr.msg_iovlen;

    let msg_ctrl = format_ptr(msghdr.msg_control as u64);
    let msg_ctrl_len = msghdr.msg_controllen;

    // ignore msg_flags as they don't appear to ever be set

    format!(
        "{{name: {name}, name_len: {name_len}, msg_iov: {msg_iov}, msg_iov_len: {msg_iov_len}, \
             msg_ctrl: {msg_ctrl}, msg_ctrl_len: {msg_ctrl_len}"
    )
}

fn format_socklevel(level: u64) -> &'static str {
    match level as c_int {
        libc::SOL_SOCKET => "SOL_SOCKET",
        libc::IPPROTO_TCP => "IPPROTO_TCP",
        libc::IPPROTO_IP => "IPPROTO_IP",
        libc::IPPROTO_IPV6 => "IPPROTO_IPV6",
        libc::SO_TYPE => "SO_TYPE",
        libc::SOL_UDP => "SOL_UDP",
        _ => "(unknown)",
    }
}

// there are probably a few missing here, but it includes all the ones
// I've found in the wild and some more
fn format_sockoptname(optname: u64) -> &'static str {
    match optname as c_int {
        libc::IP6T_SO_ORIGINAL_DST => "IP6T_SO_ORIGINAL_DST",
        libc::IPV6_DONTFRAG => "IPV6_DONTFRAG",
        libc::IPV6_RECVERR => "IPV6_RECVERR",
        libc::IPV6_RECVPKTINFO => "IPV6_RECVPKTINFO",
        libc::IPV6_TCLASS => "IPV6_TCLASS",
        libc::IPV6_UNICAST_HOPS => "IPV6_UNICAST_HOPS",
        libc::IPV6_V6ONLY => "IPV6_V6ONLY",
        libc::IP_DROP_MEMBERSHIP => "IP_DROP_MEMBERSHIP",
        libc::IP_MTU => "IP_MTU",
        libc::IP_RECVERR => "IP_RECVERR",
        libc::IP_TOS => "IP_TOS",
        libc::IP_TRANSPARENT => "IP_TRANSPARENT",
        libc::SO_ACCEPTCONN => "SO_ACCEPTCONN",
        libc::SO_BROADCAST => "SO_BROADCAST",
        libc::SO_DONTROUTE => "SO_DONTROUTE",
        libc::SO_ERROR => "SO_ERROR",
        libc::SO_KEEPALIVE => "SO_KEEPALIVE",
        libc::SO_LINGER => "SO_LINGER",
        libc::SO_OOBINLINE => "SO_OOBINLINE",
        libc::SO_PEERCRED => "SO_PEERCRED",
        libc::SO_PRIORITY => "SO_PRIORITY",
        libc::SO_RCVBUF => "SO_RCVBUF",
        libc::SO_RCVBUFFORCE => "SO_RCVBUFFORCE",
        libc::SO_RCVTIMEO => "SO_RCVTIMEO",
        libc::SO_REUSEADDR => "SO_REUSEADDR",
        libc::SO_REUSEPORT => "SO_REUSEPORT",
        libc::SO_RXQ_OVFL => "SO_RXQ_OVFL",
        libc::SO_SNDBUF => "SO_SNDBUF",
        libc::SO_SNDBUFFORCE => "SO_SNDBUFFORCE",
        libc::SO_SNDTIMEO => "SO_SNDTIMEO",
        libc::SO_TIMESTAMP => "SO_TIMESTAMP",
        libc::SO_TIMESTAMPING => "SO_TIMESTAMPING",
        libc::SO_TIMESTAMPNS => "SO_TIMESTAMPNS",
        libc::SO_TXTIME => "SO_TXTIME",
        libc::SO_TYPE => "SO_TYPE",
        libc::TCP_USER_TIMEOUT => "TCP_USER_TIMEOUT",
        libc::UDP_GRO => "UDP_GRO",
        libc::UDP_SEGMENT => "UDP_SEGMENT",
        _ => "(unknown)",
    }
}

/// Format arrays like argv and envp that include are made of an array of pointers
/// where the last element is a null pointer.
fn format_nullable_args(session: &mut Debugger, addr: u64) -> String {
    let mut args = Vec::new();

    // only try to read the first 20 args
    for idx in 0..20 {
        let addr = addr + idx * std::mem::size_of::<*const i8>() as u64;
        let arg = format_c_str(session, addr);

        if arg == "NULL" {
            break;
        }

        args.push(arg);
    }

    format!("{args:?}")
}

impl super::Debugger {
    pub fn display(&mut self, syscall: Sysno, args: [u64; 6]) -> String {
        let mut func = String::new();

        func += &syscall.to_string();
        func += "(";

        match syscall {
            Sysno::read => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::write => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::open => print_delimited![
                func,
                format_c_str(self, args[0]),
                format_flags!(args[1] => nix::fcntl::OFlag)
            ],
            Sysno::close => print_delimited![func, format_fd(args[0])],
            Sysno::stat => print_delimited![func, format_c_str(self, args[0]), format_ptr(args[1])],
            Sysno::fstat => print_delimited![func, format_fd(args[0]), format_ptr(args[1])],
            Sysno::lstat => {
                print_delimited![func, format_c_str(self, args[0]), format_ptr(args[1])]
            }
            Sysno::poll => print_delimited![
                func,
                format_array::<PollFd>(self, args[0], args[1]),
                args[1].to_string(),
                (args[2] as c_int).to_string()
            ],
            Sysno::lseek => print_delimited![
                func,
                format_fd(args[0]),
                (args[1] as i64).to_string(),
                match args[2] as c_int {
                    libc::SEEK_SET => "SEEK_SET",
                    libc::SEEK_CUR => "SEEK_CUR",
                    libc::SEEK_END => "SEEK_END",
                    libc::SEEK_DATA => "SEEK_DATA",
                    libc::SEEK_HOLE => "SEEK_HOLE",
                    _ => "(unknown)",
                }
            ],
            Sysno::mmap => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::ProtFlags),
                format_flags!(args[3] => nix::sys::mman::MapFlags),
                format_fd(args[4]),
                args[5].to_string()
            ],
            Sysno::mprotect => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::ProtFlags)
            ],
            Sysno::munmap => {
                print_delimited![func, format!("{:x}", args[0]), &args[1].to_string()]
            }
            Sysno::brk => print_delimited![func, format_ptr(args[0])],
            Sysno::rt_sigaction => print_delimited![
                func,
                match signal::Signal::try_from(args[0] as c_int) {
                    Ok(s) => s.as_str(),
                    Err(..) => "(unknown)",
                },
                format_sigaction(self, args[1]),
                format_sigaction(self, args[2])
            ],
            Sysno::rt_sigprocmask => print_delimited![
                func,
                match args[0] as c_int {
                    libc::SIG_BLOCK => "SIG_BLOCK",
                    libc::SIG_UNBLOCK => "SIG_UNBLOCK",
                    libc::SIG_SETMASK => "SIG_SETMASK",
                    _ => "(unknown)",
                },
                format_sigset(self, args[1]),
                format_sigset(self, args[2])
            ],
            Sysno::rt_sigreturn => print_delimited![],
            Sysno::ioctl => print_delimited![
                func,
                format_fd(args[0]),
                format_ioctl(args[1]),
                format_ptr(args[2])
            ],
            Sysno::pread64 => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string(),
                (args[3] as i64).to_string()
            ],
            Sysno::pwrite64 => print_delimited![
                func,
                format_fd(args[0]),
                format_str(self, args[1], args[2]),
                args[2].to_string(),
                (args[3] as i64).to_string()
            ],
            Sysno::readv => print_delimited![
                func,
                format_fd(args[0]),
                format_array::<IoVec>(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::writev => print_delimited![
                func,
                format_fd(args[0]),
                format_array::<IoVec>(self, args[1], args[2]),
                args[2].to_string()
            ],
            Sysno::access => print_delimited![
                func,
                format_c_str(self, args[0]),
                format_flags!(args[1] => nix::unistd::AccessFlags)
            ],
            Sysno::pipe => print_delimited![func, format_array::<Fd>(self, args[0], 2)],
            Sysno::pipe2 => print_delimited![
                func,
                format_array::<Fd>(self, args[0], 2),
                format_flags!(args[1] => nix::fcntl::OFlag)
            ],
            Sysno::select => print_delimited![
                func,
                args[0].to_string(),
                format_fdset(self, args[1]),
                format_fdset(self, args[2]),
                format_fdset(self, args[3]),
                format_ptr(args[4])
            ],
            Sysno::pselect6 => print_delimited![
                func,
                args[0].to_string(),
                format_fdset(self, args[1]),
                format_fdset(self, args[2]),
                format_fdset(self, args[3]),
                format_ptr(args[4]),
                format_sigset(self, args[5])
            ],
            Sysno::sched_yield => print_delimited![],
            Sysno::mremap => {
                if args[3] as i32 & libc::MREMAP_FIXED == libc::MREMAP_FIXED {
                    print_delimited![
                        func,
                        format_ptr(args[0]),
                        args[1].to_string(),
                        args[2].to_string(),
                        format_flags!(args[3] => nix::sys::mman::MRemapFlags),
                        format_ptr(args[4])
                    ]
                } else {
                    print_delimited![
                        func,
                        format_ptr(args[0]),
                        args[1].to_string(),
                        args[2].to_string(),
                        format_flags!(args[3] => nix::sys::mman::MRemapFlags)
                    ]
                }
            }
            Sysno::msync => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_flags!(args[2] => nix::sys::mman::MsFlags)
            ],
            Sysno::mincore => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                format_bytes_u8(self, args[2], args[1])
            ],
            Sysno::madvise => print_delimited![
                func,
                format_ptr(args[0]),
                args[1].to_string(),
                match args[2] as c_int {
                    libc::MADV_NORMAL => "MADV_NORMAL",
                    libc::MADV_RANDOM => "MADV_RANDOM",
                    libc::MADV_SEQUENTIAL => "MADV_SEQUENTIAL",
                    libc::MADV_WILLNEED => "MADV_WILLNEED",
                    libc::MADV_DONTNEED => "MADV_DONTNEED",
                    libc::MADV_REMOVE => "MADV_REMOVE",
                    libc::MADV_DONTFORK => "MADV_DONTFORK",
                    libc::MADV_DOFORK => "MADV_DOFORK",
                    libc::MADV_HWPOISON => "MADV_HWPOISON",
                    libc::MADV_MERGEABLE => "MADV_MERGEABLE",
                    libc::MADV_UNMERGEABLE => "MADV_UNMERGEABLE",
                    libc::MADV_SOFT_OFFLINE => "MADV_SOFT_OFFLINE",
                    libc::MADV_HUGEPAGE => "MADV_HUGEPAGE",
                    libc::MADV_NOHUGEPAGE => "MADV_NOHUGEPAGE",
                    libc::MADV_DONTDUMP => "MADV_DONTDUMP",
                    libc::MADV_DODUMP => "MADV_DODUMP",
                    libc::MADV_FREE => "MADV_FREE",
                    _ => "(unknown)",
                }
            ],
            Sysno::shmget => print_delimited![
                func,
                (args[0] as c_int).to_string(),
                args[1].to_string(),
                // TODO: print shmflg
                args[2].to_string()
            ],
            Sysno::shmat => print_delimited![
                func,
                // TODO: print shmid
                args[0].to_string(),
                format_ptr(args[1]),
                // TODO: print shmflg
                args[0].to_string()
            ],
            Sysno::shmctl => print_delimited![
                func,
                // TODO: print shmid
                args[0].to_string(),
                match args[1] as c_int {
                    libc::IPC_RMID => "IPC_RMID",
                    libc::IPC_SET => "IPC_SET",
                    libc::IPC_STAT => "IPC_STAT",
                    libc::IPC_INFO => "IPC_INFO",
                    _ => "(unknown)",
                },
                format_ptr(args[2])
            ],
            Sysno::dup => print_delimited![func, format_fd(args[0])],
            Sysno::dup2 => print_delimited![func, format_fd(args[0]), format_fd(args[0])],
            Sysno::pause => print_delimited![],
            Sysno::nanosleep => {
                print_delimited![func, format_timespec(self, args[0]), format_ptr(args[1])]
            }
            Sysno::getitimer => print_delimited![
                func,
                match args[0] as c_int {
                    libc::ITIMER_REAL => "ITIMER_REAL",
                    libc::ITIMER_VIRTUAL => "ITIMER_VIRUAL",
                    libc::ITIMER_PROF => "ITIMER_PROF",
                    _ => "(unknown)",
                },
                format_itimerval(self, args[1])
            ],
            Sysno::alarm => print_delimited![func, args[0].to_string()],
            Sysno::setitimer => print_delimited![
                func,
                match args[0] as c_int {
                    libc::ITIMER_REAL => "ITIMER_REAL",
                    libc::ITIMER_VIRTUAL => "ITIMER_VIRUAL",
                    libc::ITIMER_PROF => "ITIMER_PROF",
                    _ => "(unknown)",
                },
                format_itimerval(self, args[1]),
                format_itimerval(self, args[2])
            ],
            Sysno::getpid => print_delimited![],
            Sysno::sendfile => print_delimited![
                func,
                format_fd(args[0]),
                format_fd(args[1]),
                format_ptr(args[2]),
                args[3].to_string()
            ],
            Sysno::socket => print_delimited![
                func,
                match socket::AddressFamily::from_i32(args[0] as i32) {
                    Some(s) => format!("{s:?}"),
                    None => "(unknown)".to_string(),
                },
                match socket::SockType::try_from(args[1] as i32) {
                    Ok(s) => format!("{s:?}"),
                    Err(..) => "(unknown)".to_string(),
                },
                format_sock_protocol(args[2])
            ],
            Sysno::connect => print_delimited![
                func,
                format_fd(args[0]),
                format_sockaddr(self, args[1], Some(args[2] as u32)),
                args[2].to_string()
            ],
            Sysno::accept => print_delimited![
                func,
                format_fd(args[0]),
                format_sockaddr_using_len(self, args[1], args[2]),
                format_ptr(args[2])
            ],
            Sysno::sendto => print_delimited![
                func,
                format_fd(args[0]),
                format_bytes_u8(self, args[1], args[2]),
                args[2].to_string(),
                format_flags!(args[3] => nix::sys::socket::MsgFlags),
                format_sockaddr(self, args[4], Some(args[5] as u32)),
                args[5].to_string()
            ],
            Sysno::recvfrom => print_delimited![
                func,
                format_fd(args[0]),
                format_bytes_u8(self, args[1], args[2]),
                args[2].to_string(),
                format_flags!(args[3] => nix::sys::socket::MsgFlags),
                format_sockaddr_using_len(self, args[4], args[5]),
                format_ptr(args[5])
            ],
            Sysno::sendmsg => print_delimited![
                func,
                format_fd(args[0]),
                format_msghdr(self, args[1]),
                format_flags!(args[2] => nix::sys::socket::MsgFlags)
            ],
            Sysno::recvmsg => print_delimited![
                func,
                format_fd(args[0]),
                format_msghdr(self, args[1]),
                format_flags!(args[2] => nix::sys::socket::MsgFlags)
            ],
            Sysno::shutdown => print_delimited![
                func,
                format_fd(args[0]),
                match args[1] as c_int {
                    libc::SHUT_RD => "SHUT_READ",
                    libc::SHUT_WR => "SHUT_WRITE",
                    libc::SHUT_RDWR => "SHUT_RW",
                    _ => "(unknown)",
                }
            ],
            Sysno::bind => print_delimited![
                func,
                format_fd(args[0]),
                format_sockaddr(self, args[1], Some(args[2] as u32)),
                args[2].to_string()
            ],
            Sysno::listen => print_delimited![func, format_fd(args[0]), args[1].to_string()],
            Sysno::getsockname => print_delimited![
                func,
                format_fd(args[0]),
                format_sockaddr_using_len(self, args[1], args[2]),
                format_ptr(args[2])
            ],
            Sysno::getpeername => print_delimited![
                func,
                format_fd(args[0]),
                format_sockaddr_using_len(self, args[1], args[2]),
                format_ptr(args[2])
            ],
            Sysno::socketpair => print_delimited![
                func,
                match socket::AddressFamily::from_i32(args[0] as i32) {
                    Some(family) => format!("{family:?}"),
                    None => format!("(unknown address family)"),
                },
                match socket::SockType::try_from(args[1] as i32) {
                    Ok(tipe) => format!("{tipe:?}"),
                    Err(..) => format!("(unknown address family)"),
                },
                format_sock_protocol(args[2]),
                format_ptr(args[3])
            ],
            Sysno::setsockopt => print_delimited![
                func,
                format_fd(args[0]),
                format_socklevel(args[1]),
                format_sockoptname(args[2]),
                format_bytes_u8(self, args[3], args[4]),
                args[4].to_string()
            ],
            Sysno::getsockopt => print_delimited![
                func,
                format_fd(args[0]),
                format_socklevel(args[1]),
                format_sockoptname(args[2]),
                format_ptr(args[3]),
                format_ptr(args[3])
            ],
            Sysno::clone => print_delimited![
                func,
                format!(
                    "flags: {}",
                    format_flags!(args[0] & !0xff => nix::sched::CloneFlags)
                ),
                match signal::Signal::try_from((args[0] & 0xff) as c_int) {
                    Ok(s) => format!("exit_signal: {s}"),
                    Err(..) => "(unknown)".to_string(),
                },
                format!("child_stack: {}", format_ptr(args[1]))
            ],
            Sysno::fork => print_delimited![],
            Sysno::vfork => print_delimited![],
            Sysno::execve => print_delimited![
                func,
                format_c_str(self, args[0]),
                format_nullable_args(self, args[1]),
                format_nullable_args(self, args[2])
            ],
            Sysno::openat => print_delimited![
                func,
                if args[0] == 4294967196 {
                    "AT_FDCWD".to_string()
                } else {
                    format_fd(args[0])
                },
                format_c_str(self, args[1]),
                format_flags!(args[2] => nix::fcntl::OFlag)
            ],
            Sysno::set_tid_address => print_delimited![func, format_ptr(args[0])],
            Sysno::set_robust_list => {
                print_delimited![func, format_ptr(args[0]), args[1].to_string()]
            }
            Sysno::getrandom => print_delimited![
                func,
                format_bytes_u8(self, args[0], args[1]),
                args[1].to_string(),
                {
                    let has_random = args[2] as u32 & libc::GRND_RANDOM == libc::GRND_RANDOM;
                    let has_nonblock = args[2] as u32 & libc::GRND_NONBLOCK == libc::GRND_NONBLOCK;

                    match (has_random, has_nonblock) {
                        (true, true) => "GRND_NONBLOCK | GRND_RANDOM",
                        (true, false) => "GRND_RANDOM",
                        (false, true) => "GRND_NONBLOCK",
                        _ => "(empty)",
                    }
                }
            ],
            Sysno::newfstatat => print_delimited![
                func,
                format_fd(args[0]),
                format_c_str(self, args[1]),
                format_stat(self, args[2]),
                format_flags!(args[3] => nix::fcntl::AtFlags)
            ],
            Sysno::futex => print_delimited![
                func,
                format_ptr(args[0]),
                format_futex_op(args[1]),
                args[2].to_string()
            ],
            Sysno::getuid => print_delimited![],
            Sysno::syslog => print_delimited![
                func,
                match args[0] {
                    0 => "SYSLOG_ACTION_CLOSE",
                    1 => "SYSLOG_ACTION_OPEN",
                    2 => "SYSLOG_ACTION_READ",
                    3 => "SYSLOG_ACTION_READ_ALL",
                    4 => "SYSLOG_ACTION_READ_CLEAR",
                    5 => "SYSLOG_ACTION_CONSOLE_OFF",
                    6 => "SYSLOG_ACTION_CONSOLE_ON",
                    7 => "SYSLOG_ACTION_CONSOLE_LEVEL",
                    8 => "SYSLOG_ACTION_SIZE_UNREAD",
                    9 => "SYSLOG_ACTION_SIZE_BUFFER",
                    _ => "(unknown)",
                },
                format_str(self, args[1], args[2])
            ],
            Sysno::getgid => print_delimited![],
            Sysno::setuid => print_delimited![func, args[0].to_string()],
            Sysno::setgid => print_delimited![func, args[0].to_string()],
            Sysno::geteuid => print_delimited![],
            Sysno::getegid => print_delimited![],
            Sysno::setpgid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::getppid => print_delimited![],
            Sysno::getpgrp => print_delimited![],
            Sysno::setsid => print_delimited![func, args[0].to_string()],
            Sysno::setreuid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::setregid => print_delimited![func, args[0].to_string(), args[1].to_string()],
            Sysno::getgroups => print_delimited![func, args[0].to_string(), format_ptr(args[1])],
            Sysno::setgroups => print_delimited![
                func,
                args[0].to_string(),
                format_array::<c_int>(self, args[1], args[0])
            ],
            Sysno::setresuid => print_delimited![
                func,
                args[0].to_string(),
                args[1].to_string(),
                args[2].to_string()
            ],
            Sysno::getresuid => print_delimited![
                func,
                format_ptr(args[0]),
                format_ptr(args[1]),
                format_ptr(args[2])
            ],
            Sysno::setresgid => print_delimited![
                func,
                args[0].to_string(),
                args[1].to_string(),
                args[2].to_string()
            ],
            Sysno::getresgid => print_delimited![
                func,
                format_ptr(args[0]),
                format_ptr(args[1]),
                format_ptr(args[2])
            ],
            Sysno::getpgid => print_delimited![func, args[0].to_string()],
            Sysno::setfsuid => print_delimited![func, args[0].to_string()],
            Sysno::setfsgid => print_delimited![func, args[0].to_string()],
            Sysno::getsid => print_delimited![func, args[0].to_string()],
            Sysno::exit => print_delimited![func, args[0].to_string()],
            Sysno::exit_group => print_delimited![func, args[0].to_string()],
            _ => func += "..",
        }

        func += ")";
        func
    }
}
