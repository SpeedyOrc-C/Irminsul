# Irminsul Client

## 构建

本项目使用了 SvelteKit 与 TypeScript，使用 Node 与 npm 构建。

需要安装 [Node.js](https://nodejs.org/)。当然，你也可以使用你喜欢的包管理器。

```sh
npm install # 安装依赖
npm run build # 生成网页
```

结果将位于 `build` 文件夹内。

如果希望以开发模式运行，输入：
```sh
npm run dev
```

如果希望在开发的时候将本机作为服务器启动（例如用于在同一局域网下的其他设备上测试），输入：
```sh
npm run dev -- --host
```

## 关系图

对于关系图中的所有元素，都可以进行修改。操作方法如下图所示：

![](relation-graph-keymap.png)

其中，Atom 只可以进行移动。Cluster 除了可以移动之外，还可以调整长度、宽度和锚点的位置。

修改完毕之后，可以将位置导出为 JSON 或 本项目内使用的 Haskell 格式。
JSON 用于在网页中导入，Haskell 则可直接用于 `Cluster` 的 `RelationGraphLayout`。
