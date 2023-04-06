# Irminsul API Server

## 构建

需要安装 Glasgow Haskell Compiler 和 Cabal。更快捷的方法是直接通过 [GHCup](https://www.haskell.org/ghcup/) 全部装好。第一次编译或解释的时候，会自动安装依赖。但是时间可能会非常久，请耐心等待。

```sh
cabal build # 生成二进制
```

结果将位于 `dist-newstyle/build` 目录下，以操作系统、编译器版本号与项目版本号区分。

如果你想直接看看这里的每部分如何工作，则不需要编译，打开解释器玩玩吧！

```sh
cabal repl
```

## API

所有的 API 将可以在端口 50000 上访问。

试一试通过[这个链接](http://localhost:50000/api/relation-graph?id=KnightsOfFavonius&lang=zh-cn)获取简体中文的西风骑士团的数据吧：

```
http://localhost:50000/api/relation-graph?id=KnightsOfFavonius&lang=zh-cn
```
