<script lang="ts">
    const contributors = [
        "海底月", "hanae", "鹤瀚", "橘子夹", "可颂 XON",
        "桃李春风一壶酒", "阳光or桃子", "阳光´-ω-`美玉",
        "逸久罢一龄", "语颂源", "ZY_",
    ]
</script>

<h1>Irminsul</h1>

<h2>这是什么？</h2>

<p>这是一个展示电子游戏《原神》中部分人物的关系的东西。</p>

<h2>鸣谢（读音顺序）</h2>

<p>{@html contributors.join("<br>")}</p>

<h2>其他信息</h2>

<p>客户端版本：2023.0827.0</p>

<style lang="scss">
    %common {
        color: #ece4d8;
    }
    h1 {
        @extend %common;
        font-size: 3rem;
        font-family: HYWenHei-85W, sans-serif;
        font-weight: normal;

        border-left: 0.5rem solid #ffbd22;
        padding-left: 1rem;
    }
    h2 {
        @extend %common;
        font-size: 2rem;
        font-family: HYWenHei-85W, sans-serif;
        font-weight: normal;

        margin-left: 0.2rem;
        border-left: 0.3rem solid #ffbd22;
        padding-left: 1rem;
    }
    p {
        @extend %common;
        font-size: 1.2rem;
        font-family: HYWenHei-65W, sans-serif;
    }
    a {
        @extend %common;
    }
</style>
