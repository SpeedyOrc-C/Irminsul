export default class Slider {
    callback: (p: number) => void;

    slider: HTMLElement;

    min: number;
    max: number;
    divisions: number | null;

    dragging = false;

    #sliderMinX = 0;
    #sliderMaxX = 0;

    constructor(slider: HTMLElement, min: number, max: number, divisions: number | null, callback: (p: number) => void) {
        this.slider = slider;
        this.min = min;
        this.max = max;
        this.divisions = divisions;
        this.callback = callback;
    }

    start = () => {
        this.dragging = true;

        const rect = this.slider.getBoundingClientRect();
        this.#sliderMinX = rect.left;
        this.#sliderMaxX = rect.right;
    };

    drag = (cursorX: number) => {
        if (!this.dragging) return;

        const rawPercentage = (cursorX - this.#sliderMinX) / (this.#sliderMaxX - this.#sliderMinX);

        let clampedRawPercentage;

        if (rawPercentage < 0)
            clampedRawPercentage = 0;
        else if (rawPercentage > 1)
            clampedRawPercentage = 1;
        else
            clampedRawPercentage = rawPercentage;

        if (this.divisions === null) {
            this.callback(clampedRawPercentage);
        } else {
            const segmentNum = this.divisions - 1;
            const alignedPercentage = Math.round(clampedRawPercentage * segmentNum) / segmentNum;
            this.callback(alignedPercentage);
        }
    };

    stop = () => { this.dragging = false; };

    toValue = (p: number) => this.min + (this.max - this.min) * p;
    fromValue = (v: number) => (v - this.min) / (this.max - this.min);
}
