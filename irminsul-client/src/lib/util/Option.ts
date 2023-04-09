type Option = {
    label: string;
    value: string;
};

export function op(value: string, label: string): Option {
    return { label, value };
}

export { type Option };
