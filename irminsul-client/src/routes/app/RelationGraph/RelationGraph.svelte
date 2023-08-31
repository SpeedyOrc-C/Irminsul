<script lang="ts">
    import {dumpRelationGraph2Haskell, type RelationGraph,} from "./RelationGraph";
    import {saveStringAsFile} from "$lib/util/String";
    import Panel from "./Panel.svelte";
    import {afterUpdate, onMount} from "svelte";
    import Settings from "./Settings/Settings.svelte";
    import DialogOk from "$lib/ui/Dialog/DialogOk.svelte";
    import {_, locale} from "svelte-i18n";
    import {RelationGraphLoader} from "./RelationGraphLoader";
    import ViewController from "./ViewController";
    import Joystick from "./Joystick.svelte";
    import RelationGraphSettings, {ShowJoystick, WhoAmI} from "./RelationGraphSettings";
    import ViewOptions from "./ViewOptions.svelte";
    import ViewGraphical from "./ViewGraphical/ViewGraphical.svelte";
    import Editor from "./ViewGraphical/Editor";

    export let id: string;

    let view = new ViewController();
    let editor: Editor;
    let editing = false;
    let hideUi = false;

    const loader = new RelationGraphLoader();

    const settings = new RelationGraphSettings("relation_graph_settings", "2023.08.30.1357");

    let relationGraph: RelationGraph | null = null;
    let jsonFileInput: HTMLInputElement;
    let jsonFileReader: FileReader;

    let showAxis = false;
    let showGrid = false;
    let showCoordinates = false;
    let showJoystick: ShowJoystick = ShowJoystick.Never;
    let showSettings = false;
    let joystickSensibility = 4;
    let smoothMovement = true;

    let showLayoutMissing = false;

    function updateView() { view = view; }

    async function loadRelationGraph(
        loadId: string,
        language=settings.preference.language,
        whoAmI=settings.preference.who_am_i
    ) {
        console.info("Loading relation graph:", loadId);

        const previous = relationGraph;

        let json: RelationGraph;
        try {
            relationGraph = null;
            json = await loader.load(loadId, language, whoAmI);
        } catch (e) {
            console.warn("Failed to load relation graph, error:", e);
            relationGraph = previous;
            switch (e) {
                case "LayoutMissing":
                    showLayoutMissing = true;
                    break;
                default:
                    break;
            }
            return;
        }

        relationGraph = json;
        editor = new Editor(relationGraph);
        view.reset();
        updateView();
        id = loadId;

        window.history.replaceState(undefined, "", `/app/?id=${id}`);

        console.info("Relation graph loaded: ", json);
    }

    function exportHaskell() {
        if (relationGraph == null) {
            console.error("No relation graph opened.");
            return;
        }
        console.info("Saving as Haskell...");

        let fileName = `${relationGraph.id}-Haskell.txt`;
        saveStringAsFile(fileName, dumpRelationGraph2Haskell(relationGraph));
        editing = false;
    }

    function exportJson() {
        if (relationGraph == null) {
            console.error("No relation graph opened.")
            return;
        }
        console.info("Saving as JSON...");

        let fileName = `${relationGraph.id}-JSON.json`;
        saveStringAsFile(fileName, JSON.stringify(relationGraph, null, 4));
        editing = false;
    }

    function importJson() {
        console.info("Trying to import from JSON...");
        jsonFileInput.click();
    }

    function openSettings() {
        showSettings = true;
    }

    function setLanguage(language: string) {
        settings.preference.language = language;
        settings.save();
        locale.set(language);
        loadRelationGraph(id);
    }

    function setWhoAmI(who_am_i: WhoAmI) {
        settings.preference.who_am_i = who_am_i;
        settings.save();
        loader.clearCache();
        loadRelationGraph(id);
    }

    function setShowAxis(show_axis: boolean) {
        showAxis = show_axis;
        settings.preference.show_axis = show_axis;
        settings.save();
    }

    function setShowGrid(show_grid: boolean) {
        showGrid = show_grid;
        settings.preference.show_grid = show_grid;
        settings.save();
    }

    function setReduceVisualEffect(reduce_visual_effect: boolean) {
        settings.preference.reduce_visual_effect = reduce_visual_effect;
        settings.save();
    }

    function setShowJoystick(show_joystick: ShowJoystick) {
        settings.preference.show_joystick = show_joystick;
        settings.save();
    }

    function setJoystickSensibility(joystick_sensibility: number) {
        settings.preference.joystick_sensitivity = joystick_sensibility;
        settings.save();
    }

    function setEditing(editing: boolean) {
        editor.setEditing(editing);
        editor = editor;
    }

    function resetAll() {
        localStorage.clear();
        window.location.href = "/app";
    }

    onMount(() => {
        showAxis = settings.preference.show_axis;
        showGrid = settings.preference.show_grid;
        showJoystick = settings.preference.show_joystick;
        joystickSensibility = settings.preference.joystick_sensitivity;
        locale.set(settings.preference.language);

        jsonFileReader = new FileReader();

        jsonFileInput.addEventListener("change", () => {
            if (jsonFileInput.files?.length == 0) {
                console.error("No import file selected.");
                return;
            }

            const files = jsonFileInput.files!;

            if (files.length > 0) {
                const file = files[0];
                jsonFileReader.readAsText(file);
            }
        });

        jsonFileReader.addEventListener(
            "load",
            (event: ProgressEvent<FileReader>) => {
                const text = event.target?.result as string;
                try {
                    const layout = JSON.parse(text);
                    relationGraph = layout;
                    console.info("Relation graph updated with", layout);
                } catch (e) {
                    console.error(
                        "Failed to parse JSON file.",
                        event.target?.result
                    );
                }
            }
        );

        loadRelationGraph(id);
    });

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey !== e.metaKey && e.code === "Comma") {
            openSettings();
        } else if (e.code == "KeyH") {
            hideUi = !hideUi;
        }
    }

    function beforeunload(e: BeforeUnloadEvent) {
        if (editor.isEditing()) {
            e.preventDefault();
            e.returnValue = "";
        }
    }

    afterUpdate(() => {
        if (editor !== undefined) {
            if (editing !== editor.isEditing()) {
                setEditing(editing);
            }
        }
    })
</script>

<svelte:window on:keydown={keydown} on:beforeunload={beforeunload} />

<title>
    Irminsul
    {#if relationGraph != null}
        - {relationGraph.rootTranslation}
    {/if}
</title>

<div id="relation-graph">
    {#if relationGraph !== null}
        <ViewGraphical bind:relationGraph
               bind:view
               bind:editor
               bind:showAxis on:set-show-axis={e => setShowAxis(e.detail)}
               bind:showGrid on:set-show-grid={e => setShowGrid(e.detail)}
               bind:showCoordinates
               bind:hideUi
               on:jump-to={e => loadRelationGraph(e.detail)}
               {settings} {smoothMovement}
        />
    {/if}

    <Joystick callback={(x, y, dx, dy) => view.joystickEvent(x, y, dx, dy, 0.005 * (0.25 + joystickSensibility / 4))}
              bind:showJoystick={settings.preference.show_joystick}
              bind:hideUi
              bind:smoothMovement
              on:update-view={updateView}/>

    <ViewOptions bind:showCoordinates bind:editing bind:hideUi />

    <Panel on:jump-to={e => loadRelationGraph(e.detail)}
           on:open-settings={openSettings}
           bind:relationGraph
           bind:hideUi
           {editing} {id}/>

    <DialogOk title={$_("error.layout-missing.self")} bind:show={showLayoutMissing}>
        {$_("error.layout-missing.detail")}
    </DialogOk>

    <Settings {settings}
              bind:show={showSettings}
              on:import-json={importJson}
              on:export-json={exportJson}
              on:export-haskell={exportHaskell}
              on:set-show-axis={e => setShowAxis(e.detail)} bind:showAxis
              on:set-show-grid={e => setShowGrid(e.detail)} bind:showGrid
              on:set-show-joystick={e => setShowJoystick(e.detail)} bind:showJoystick
              on:set-joystick-sensibility={e => setJoystickSensibility(e.detail)} bind:joystickSensibility
              on:set-reduce-visual-effect={e => setReduceVisualEffect(e.detail)}
              on:set-language={e => setLanguage(e.detail)}
              on:set-who-am-i={e => setWhoAmI(e.detail)}
              on:reset-all={resetAll}
    />

    <label for="json-file-input" />
    <input id="json-file-input" type="file" bind:this={jsonFileInput} style:display="none" />
</div>

<style lang="scss">
    #relation-graph {
        height: 100vh;
        width: 100vw;

        touch-action: manipulation;
    }
</style>
