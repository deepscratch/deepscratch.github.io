---
title: 排查 Path MTU 问题
---

近来一直发现深圳的机器连接北京的系统时，如果数据包比较大就会出现服务器响应超时。
问题发现后，首先使用深圳的 Mikrotik 设备上的 http client 发送了一个大包进行排
查。为了构造一个较大的数据包，在 GET 的参数里添加一些无用的数据。

```bash
/tool fetch url="http://10.0.8.32:8810/index.html\?id=1111...
```

然后在 Mikrotik 端和 ERP 的 NGINX 端都启动了抓包程序。在 NGINX 端抓包发现一旦
tcpdump 显示的数据包大小超过 1390 。三次握手的由 Mikrotik 发送的最后一个 ACK
包（这个包同时带有 PSH Flag 并且还携带了数据），就无法被收到。Mikrotik 端抓包
显示这个 ACK 包在不停被重传。由于问题的复现依赖于数据包的大小，因此感觉到是
Path MTU 的问题，于是开始使用不同大小的 ICMP 包进行 ping 测试

```bash
/ping VPN网关地址 size=1392
```

测试后发现只要是 size > 1392 包就会被丢弃。感觉像是链路上有个地方的 MTU 配置出
错了。（考虑到 PPPoE 的默认 MTU 是 1492，1392很可能是被打错了）

最终无奈，只能将 Mikrotik 设备上 PPPoE 接口的 MTU 缩小，以解决这个问题。


如果使用 Linux 进行 Path MTU 测试可以使用这个命令，

```bash
ping -M dont -s 1464 114.114.114.114
```

`-s` 用来指定包的大小。`-M dont` 给 ICMP 包夹带 `don't fragment` 标志位，让路
由器直接丢弃过大的 ICMP 包。
