export default class Slider {
    slider: HTMLElement;

    min: number;
    max: number;
    value: number;

    divisions: number | null;
    percentage = 0;

    dragging = false;

    private sliderMinX = 0;
    private sliderMaxX = 0;

    constructor(slider: HTMLElement, min: number, max: number, value: number, divisions: number | null) {
        if (divisions !== null && divisions < 2)
            throw new Error("Divisions of a slider must be at least 2.");

        this.slider = slider;
        this.min = min;
        this.max = max;
        this.divisions = divisions;
        this.value = value;
        this.percentage = this.fromValue(value);
    }

    start = () => {
        this.dragging = true;

        const rect = this.slider.getBoundingClientRect();
        this.sliderMinX = rect.left;
        this.sliderMaxX = rect.right;
    };

    drag = (cursorX: number) => {
        if (!this.dragging) return;

        const rawPercentage = (cursorX - this.sliderMinX) / (this.sliderMaxX - this.sliderMinX);
        this.percentage = this.align(this.clamp(rawPercentage));
        this.value = this.toValue(this.percentage);
    };

    increase = () => {
        if (this.divisions === null || this.percentage == 1) return false;

        const segmentNum = this.divisions - 1;
        this.percentage = this.clamp(Math.round(this.percentage * segmentNum + 1) / segmentNum);
        this.value = this.toValue(this.percentage);

        return true;
    }

    decrease = () => {
        if (this.divisions === null || this.percentage == 0) return false;

        const segmentNum = this.divisions - 1;
        this.percentage = this.clamp(Math.round(this.percentage * segmentNum - 1) / segmentNum);
        this.value = this.toValue(this.percentage);

        return true;
    }

    setPercentage = (p: number) => {
        this.percentage = this.align(this.clamp(p));
        this.value = this.toValue(this.percentage);
    }

    stop = () => { this.dragging = false; };

    toValue = (p: number) => this.min + (this.max - this.min) * p;

    private fromValue = (v: number) => (v - this.min) / (this.max - this.min);

    private clamp(p: number) {
        if (p < 0) return 0;
        else if (p > 1) return 1;
        return p;
    }

    private align(p: number) {
        if (this.divisions === null) return p;
        const segmentNum = this.divisions - 1;
        return Math.round(p * segmentNum) / segmentNum;
    }
}
