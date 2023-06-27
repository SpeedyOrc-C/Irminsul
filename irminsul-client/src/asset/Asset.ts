import img_avatar_UnknownAvatar from './img/avatar/_UnknownAvatar.png'

export const asset: Record<string, () => Promise<any>> = import.meta.glob('./**/*')

export function getImgAvatar(id: string, assign: (result: string) => void) {
    const path = `./img/avatar/${id}.png`
    try {
        asset[path]()
            .then(result => {
                console.log(result);
                assign(result.default);
            })
            .catch(() => assign(img_avatar_UnknownAvatar))
    } catch (e) {
        assign(img_avatar_UnknownAvatar)
    }
}

export {
    img_avatar_UnknownAvatar,
} 
