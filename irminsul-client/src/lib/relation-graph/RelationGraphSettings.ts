type BasePreference = { readonly version: string; }

interface ISettings<P> {
    name: string;
    version: string;
    preference: P | undefined;
    getDefault: () => P;
    load: () => boolean;
    save: () => void;
    reset: () => void;
}

abstract class BaseSettings<P extends BasePreference> implements ISettings<P> {
    name: string;
    version: string;
    preference!: P;

    constructor(name: string, version: string) {
        this.name = name;
        this.version = version;
        this.load();
        this.save();
    }

    abstract getDefault(): P;

    load = () => {
        // Does preference exist?
        const rawPreference = localStorage.getItem(this.name);
        if (rawPreference === null) {
            console.info("Preference does not exist. Creating a default one...");
            this.preference = this.getDefault();
            return false;
        }

        let jsonPreference: P;

        // Parsed successfully?
        try {
            jsonPreference = JSON.parse(rawPreference);
        } catch (e) {
            console.info("Cannot parse preference. Creating a default one...");
            this.preference = this.getDefault();
            return false;
        }

        // Is an outdated version?
        if (jsonPreference.version < this.version) {
            console.info("This preference is outdated. Creating a updated default one...");
            this.preference = this.getDefault();
            return false;
        }

        console.info("Preference loaded:", jsonPreference);

        this.preference = jsonPreference;
        return true;
    }

    save = () => {
        localStorage.setItem(this.name, JSON.stringify(this.preference));
    }

    reset = () => {
        this.preference = this.getDefault();
        this.save();
    }
}

export enum WhoAmI { Lumine="lumine",  Aether="aether"}
export enum ShowJoystick { Never="never", HasCoarsePointer="has-coarse-pointer", Always="always" }

type RelationGraphPreference = BasePreference & {
    language: string;
    who_am_i: WhoAmI;
    show_axis: boolean;
    show_grid: boolean;
    show_joystick: ShowJoystick;
    joystick_sensitivity: number;
    reduce_visual_effect: boolean;
}

export default class RelationGraphSettings extends BaseSettings<RelationGraphPreference> {
    getDefault() {
        return {
            version: this.version,
            language: 'en-us',
            who_am_i: WhoAmI.Aether,
            show_axis: false,
            show_grid: false,
            show_joystick: ShowJoystick.HasCoarsePointer,
            joystick_sensitivity: 4,
            reduce_visual_effect: true,
        }
    }
}
