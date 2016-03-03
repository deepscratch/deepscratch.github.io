---
title: PostgreSQL 里 work_mem 的配置
---

今天一台 postgresql 机器 swap 报警。大约占用了 2GB 的 swap 空间。登陆机器后，
首先通过 `/proc` 文件系统找到了使用 swap 较多的进程。

```bash
grep  'VmSwap:' /proc/*/status   | sort -n -k 2
```

发现 swap 占用最多的进程都是 postgres 的服务进程。然后在核实了系统的
`vm.swappiness` 确实已经设置为 0 的情况下，开始怀疑是 postgres 配置了过多的内
存。

考虑到 swap 是用来交换系统匿名页(anonymous pages)的，匿名页是通过 malloc 系列
函数产生的内存分配，因此怀疑是 postgres 的 `work_mem` 配置有问题。查看
postgres.conf 后发现，work_mem 被配置成了 10GB 。虽然系统内存总共是 128 GB，但
是按照
[PostgreSQL 文档](http://www.postgresql.org/docs/9.4/static/runtime-config-resource.html")
的描述，每个服务进程至少会分配 work_mem 大小的内存进行排序等操作。通过 netstat
-ant 了解到机器上的服务进程一共有将近 200 个，并且大多用于进行统计等业务。这也
基本符合 `work_mem` 设置过大从而导致内存使用过多的特征。

对于 work_mem 文档中做了以下描述：

> Note that for a complex query, several sort or hash operations might be
> running in parallel; each operation will be allowed to use as much memory as
> this value specifies before it starts to write data into temporary files

即可并行化的 SQL 会导致多份 work_mem 大小的内存被分配。因此，文档中也特别强调，

> Therefore, the total memory used could be many times the value of work_mem;
