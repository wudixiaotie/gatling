su
# max length of tcp syn queue
# backlog
echo 30000 > /proc/sys/net/core/somaxconn
echo 30000 > /proc/sys/net/core/netdev_max_backlog
# max connection at one port
echo "*         soft    nofile  100000" >> /etc/security/limits.conf
echo "*         hard    nofile  100000" >> /etc/security/limits.conf
# only for test
echo "1024 65535" > /proc/sys/net/ipv4/ip_local_port_range