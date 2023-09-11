<script lang="ts">
    import {_, locale} from "svelte-i18n";
    import Name from "./Name";
    import NameC from "./Name.svelte";

    const dataProviders: Array<Name> = [
        new Name("冰封勿忘", "Bīngfēng Wùwàng", "https://space.bilibili.com/7543512"),
        new Name("陈湛明", "Chén Zhànmíng", "https://github.com/SpeedyOrc-C"),
        new Name("deepyami", null, "https://space.bilibili.com/10989654"),
        new Name("语颂源", "Etymodes", "https://space.bilibili.com/25150765"),
        new Name("海底月", "Hǎidǐyuè"),
        new Name("hanae"),
        new Name("大锅儿", "Dàguōr", "https://space.bilibili.com/12472683"),
        new Name("橘子夹", "Júzijiá", "https://space.bilibili.com/470362187"),
        new Name("可颂 XON", "Kěsòng XON", "https://space.bilibili.com/259103285"),
        new Name("桃李春风一壶酒", "Táolǐ Chūnfēng Yī-Hú Jiǔ"),
        new Name("阳光or桃子", "Yángguāng or Táozi"),
        new Name("阳光´-ω-`美玉", "Yángguāng Měiyù"),
        new Name("逸久罢一龄", "Yìjiǔbàyīlíng"),
        new Name("ZY_", null, "https://space.bilibili.com/67662455"),
    ]

    const version = "2023.0911.0";
</script>

<!-- svelte-ignore a11y-no-noninteractive-tabindex -->
<div class="about" tabindex="0">
    <h1>Irminsul</h1>

    <h2>{$_("about.what-is-this.title")}</h2>
    <p>{$_("about.what-is-this.body")}</p>

    <h2>{$_("about.acknowledgements.title")}</h2>

    <h3>{$_("about.acknowledgements.data-provider")}</h3>
    <p>
        {#each dataProviders as name}
            <NameC {name}/>
            <br>
        {/each}
    </p>

    <h3>{$_("about.acknowledgements.software-used.title")}</h3>
    <p>
        {#if $locale === "zh-CN"}
            本网页由
            <a href="https://jb.gg/OpenSourceSupport" class="text" target="_blank">
                开源开发许可证
            </a>
            下的 WebStorm 编写。
        {:else if $locale === "en-US"}
            This website is developed with Webstorm under
            <a href="https://jb.gg/OpenSourceSupport" class="text" target="_blank">
                Open Source Development License.
            </a>
        {/if}
    </p>
    <p></p>
    <p><a href="https://www.jetbrains.com/webstorm" target="_blank">
        <img class="webstorm-logo" alt="WebStorm logo."
             src="https://resources.jetbrains.com/storage/products/company/brand/logos/WebStorm.png">
    </a></p>
    <p class="copyright">{@html $_("about.acknowledgements.software-used.webstorm.copyright")}
    </p>

    <p>{$_("about.client-version") + version}</p>
</div>

<style lang="scss">
    @import "./global.scss";

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

    h3 {
        @extend %common;
        font-size: 1.5rem;
        font-family: HYWenHei-85W, sans-serif;
        font-weight: normal;

        margin-left: 0.5rem;
        padding-left: 1rem;
    }

    p {
        @extend %common;
        font-size: 1.2rem;
        font-family: HYWenHei-65W, sans-serif;
        padding-left: 1.5rem;
    }

    .webstorm-logo {
        background-color: #ece4d8;
        border-radius: 4px;
        width: 20rem;
    }

    .copyright {
        font-size: 0.8rem;
    }
</style>