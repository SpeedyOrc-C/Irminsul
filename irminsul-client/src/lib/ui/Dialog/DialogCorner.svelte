<script>
    import Star from "../Star.svelte";
</script>

<div class="corner">
    <div class="big-arc1" />
    <div class="big-arc2" />
    <div class="small-arc" />

    <div class="corner-shadow" />
    <div class="corner-border" />
    <div class="star">
        <Star />
    </div>
</div>

<style lang="scss">
    .corner {
        position: absolute;

        %big-arc {
            position: absolute;
            width: 0.4rem;
            height: 0.4rem;
            border-radius: 0.4rem 0 0 0;
            border-top: 0.15rem solid #51545d;
            border-left: 0.15rem solid #51545d;
        }

        & > .big-arc1 {
            @extend %big-arc;
            top: 0.9rem;
            left: 1.5rem;
        }

        & > .big-arc2 {
            @extend %big-arc;
            top: 1.5rem;
            left: 0.9rem;
        }

        & > .small-arc {
            position: absolute;

            height: 0.1rem;
            width: 0.1rem;
            top: 1.4rem;
            left: 1.4rem;

            border-radius: 0 0 0.4rem 0;
            border-bottom: 0.15rem solid #51545d;
            border-right: 0.15rem solid #51545d;
        }

        & > .corner-shadow {
            position: absolute;
            transform: translate(0.75rem, 0.75rem);
            box-shadow: 0 0 0.75rem 0.75rem #f808;
        }

        & > .corner-border {
            position: absolute;
            height: 1.3rem;
            width: 1.3rem;

            overflow: hidden;

            border-top: 0.25rem solid #feeeae;
            border-left: 0.25rem solid #feeeae;
            border-radius: 0.25rem;

            &::after {
                position: absolute;
                width: 2.6rem;
                height: 2.6rem;

                content: "";
                border-radius: 1.3rem;
                box-shadow: 0 0 0 0.65rem #ebba7c;
            }
        }

        & > .star {
            position: absolute;
            height: 1.6rem;
            width: 1.6rem;
            top: 0.1rem;
            left: 0.1rem;

            fill: #feeeae;
            transform: rotate(45deg);
        }
    }
</style>
