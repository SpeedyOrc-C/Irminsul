# Irminsul API

## 构建

本项目使用了 Haskell，使用 Cabal 构建。尤其是在 Windows 中，安装这些东西极具挑战性。

需要安装 Glasgow Haskell Compiler 和 Cabal。
更快捷的方法是直接通过 [GHCup](https://www.haskell.org/ghcup/) 全部装好。
第一次编译或解释的时候，会自动安装依赖，但是时间可能会非常久，请耐心等待。

```sh
cabal update # 更新库索引
cabal build # 生成可执行文件
```

结果将位于 `dist-newstyle/build` 目录下，以操作系统、编译器版本号与项目版本号区分。
也可以直接构建，然后运行：

```sh
cabal run
```

如果你想直接看看这里的每部分如何工作，则不需要编译，打开解释器玩玩吧！

```sh
cabal repl
```

## API

所有的 API 将可以在端口 50000 上访问。

### 关系图

#### 格式

```
/api/relation-graph/<集合ID>/<语言代码>/<主角>
```

#### 例子

简体中文的以空为视角的西风骑士团的数据：

```
http://localhost:50000/api/relation-graph/KnightsOfFavonius/zh-cn/aether
```
