<script lang="ts">
    import { page } from "$app/stores";
    import { onMount } from "svelte";
    import type { ApiResponse } from "../../model/Api";
    import type { EntityRelations as TEntityRelations } from "../../model/EntityRelations";
    import EntityRelations from "./EntityRelations.svelte";

    let entityRelationsResponse: ApiResponse<TEntityRelations> | null;
    let entityRelations: TEntityRelations | undefined;
    let id: string | null;
    let lang: string | null;

    onMount(() => {
        id = $page.url.searchParams.get("id");
        lang = $page.url.searchParams.get("lang");
        if (id == null || lang == null) return;

        fetch(`/api/entity-relations?id=${id}&lang=${lang}`).then(r => {
            r.json().then(json => {
                entityRelationsResponse = json
                entityRelations = entityRelationsResponse?.body
            })
        })
    })
</script>

{#if id == null || lang == null}
    <p>Missing parameters "id" or "lang"</p>
{:else if entityRelationsResponse === undefined}
    <p>Loading...</p>
{:else if entityRelationsResponse == null}
    <p>Cannot parse fetched result</p>
{:else if entityRelationsResponse.status === "UnsupportedLanguage"}
    <p>Language "{lang}" is not supported</p>
{:else if entityRelationsResponse.status === "NotImplementedEntity"}
    <p>Entity "{id}" is not yet implemented</p>
{:else if entityRelationsResponse.status === "OK" && entityRelations !== undefined}
    <EntityRelations {entityRelations} />
{:else}
    <p>UNKNOWN ERROR</p>
{/if}

<style>
    :global(body) {
        background-color: #171f2b;
    }
    
    @font-face {
        font-family: HYWenHei-65W;
        src: url("/asset/font/HYWenHei-65W.ttf");
    }

    @font-face {
        font-family: HYWenHei-85W;
        src: url("/asset/font/HYWenHei-85W.ttf");
    }

    :global(.font-hywh-65w) {
        font-family: HYWenHei-65W, sans-serif;
    }

    :global(.font-hywh-85w) {
        font-family: HYWenHei-85W, sans-serif;
    }
</style>
