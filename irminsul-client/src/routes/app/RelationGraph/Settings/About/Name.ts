export default class Name {
    public native: string;
    public roman: string | null;
    public link: string | null = null;
    constructor(native: string, roman: string | null = null, link: string | null = null) {
        this.native = native;
        this.roman = roman;
        this.link = link;
    }
}