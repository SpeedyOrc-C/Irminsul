<script lang="ts">
    import {afterUpdate} from "svelte";
    import type { Vector2 } from "$lib/util/Vector2";
    import type {RelationBetween} from "../../RelationGraph";
    import type Editor from "../Editor";
    import type RelationGraphSettings from "../../RelationGraphSettings";
    import RelationText from "./RelationText.svelte";

    export let relationBetween: RelationBetween;
    export let editor: Editor;
    export let settings: RelationGraphSettings;

    let highlight = false;
    let dim = false;
    let flip = false;
    let cjkRotateLeft = false;

    let width: number;
    let position: Vector2 = { x: 0, y: 0 };
    let rotation: number;

    let forwardRelationY: number;
    let backwardRelationY: number;

    afterUpdate(() => {
        highlight = editor.isSelected(relationBetween.subjectId)
                 || editor.isSelected(relationBetween.objectId);

        dim = !editor.isEditing() && !highlight && editor.numSelected() > 0;

        const subjectAnchor = editor.anchorOf(relationBetween.subjectId);
        const objectAnchor = editor.anchorOf(relationBetween.objectId);
        const ox = objectAnchor.x;
        const oy = objectAnchor.y;
        const sx = subjectAnchor.x;
        const sy = subjectAnchor.y;
        const y = oy - sy;
        const x = ox - sx;

        if (settings.preference.language === "zh-cn") {
            flip = x <= 0 && y >= x || x > 0 && y > x;
            cjkRotateLeft = y > x && y > -x || y < x && y < -x;
        } else {
            flip = x < 0 || (x == 0 && y > 0);
        }

        width = Math.sqrt(x ** 2 + y ** 2);

        position.x = (sx + ox) / 2;
        position.y = (sy + oy) / 2;
        rotation = -Math.atan((sy - oy) / (sx - ox)) - (ox <= sx ? Math.PI : 0);

        const hasBiRelation = relationBetween.biRelations.length > 0;

        // Bi-relations are always centered. But uni-directional relations' offset can change for better readability.
        if (hasBiRelation) {
            // Uni-directional relations are placed
            // above or below the bi-relations.
            forwardRelationY = 1.4;
            backwardRelationY = -1.4;
        } else {
            const hasBackwardRelation = relationBetween.backwardRelations.length > 0;
            const hasForwardRelation = relationBetween.forwardRelations.length > 0;
            // If there is only one uni-directional relation
            if (hasForwardRelation !== hasBackwardRelation) {
                // It is placed in the middle.
                forwardRelationY = 0;
                backwardRelationY = 0;
            } else {
                // If there are two uni-directional relations,
                // they are placed above and below the middle.
                forwardRelationY = 0.7;
                backwardRelationY = -0.7;
            }
        }
    });
</script>

<div class="relation-between" class:highlight class:dim class:flip
    style:width="{width}rem"
    style:left="{position.x}rem"
    style:top="{-position.y}rem"
    style:transform="translate(-50%, -50%) rotate({rotation}rad)"
>
    {#if relationBetween.biRelations.length > 0}
        <div class="bi-relation font-hywh-65w">
            <div class="relations">
                {#each relationBetween.biRelations as relation}
                    <div class="relation">
                        <RelationText {relation} {flip} {cjkRotateLeft} />
                    </div>
                {/each}
            </div>
        </div>
    {/if}

    {#if relationBetween.forwardRelations.length > 0}
        <div class="forward-relation font-hywh-65w"
             style:top="calc(50% + {-forwardRelationY}rem)">
            <div class="relations">
                {#each relationBetween.forwardRelations as relation}
                    <div class="relation">
                        <RelationText {relation} {flip} {cjkRotateLeft} />
                    </div>
                {/each}
            </div>
        </div>
    {/if}

    {#if relationBetween.backwardRelations.length > 0}
        <div class="backward-relation font-hywh-65w"
             style:top="calc(50% + {-backwardRelationY}rem)">
            <div class="relations">
                {#each relationBetween.backwardRelations as relation}
                    <div class="relation">
                         <RelationText {relation} {flip} {cjkRotateLeft} />
                    </div>
                {/each}
            </div>
        </div>
    {/if}
</div>

<style lang="scss">
    .relation-between {
        position: absolute;
        transform: translate(-50%, -50%);

        height: 5.4rem;
        z-index: 1;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        &:hover,
        &:active {
            z-index: 2;
            background-color: #bda27733;
        }

        transition-property: filter;
        transition-duration: 0.2s;

        &.dim {
            filter: brightness(50%) blur(0.1rem);

            &:hover, &:active {
                filter: none;
            }
        }

        &.highlight {
            z-index: 2;
        }
    }

    %relation-shared {
        position: absolute;
        transform: translate(-50%, -50%);
        left: 50%;

        width: 100%;
        height: 1.1rem;
        line-height: 1.1rem;
        padding: 0.1rem 0;

        border: 0.1rem solid #bda277;

        font-size: 0.8rem;
    }

    .bi-relation {
        @extend %relation-shared;
        top: 50%;
        background-color: #3b4255;
        color: white;
        text-align: center;
    }

    %uni-relation-shared {
        @extend %relation-shared;
        background-color: #f5ece1;
        color: #3b4255;
    }

    .forward-relation {
        @extend %uni-relation-shared;

        background-image: linear-gradient(
            90deg,
            #ffbd22aa 0%,
            transparent 10rem,
            transparent 100%
        );

        & > div {
            position: absolute;
            left: 3rem;
        }
    }

    .backward-relation {
        @extend %uni-relation-shared;

        background-image: linear-gradient(
            90deg,
            transparent 0%,
            transparent calc(100% - 10rem),
            #ffbd22aa 100%
        );

        & > div {
            position: absolute;
            right: 3rem;
        }
    }

    .relation {
        display: inline-block;
    }

    .relation:nth-child(n+2) {
        margin-left: 0.3rem;
        border-left: 0.1rem solid #0004;
        padding-left: 0.3rem;
    }
</style>
