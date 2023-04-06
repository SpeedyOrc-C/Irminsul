import img_ui_background_root_cluster from './img/ui/background-root-cluster.png'
import img_avatar_UnknownAvatar from './img/avatar/_UnknownAvatar.png'

export const asset: Record<string, () => Promise<any>> = import.meta.glob('./**/*')

export function getImgAvatar(id: string, assign: (result: string) => void) {
    let path = `./img/avatar/${id}.png`
    if (asset.hasOwnProperty(path)) {
        asset[path]()
            .then(result => assign(result.default))
            .catch(() => assign(img_avatar_UnknownAvatar))
    } else {
        assign(img_avatar_UnknownAvatar)
    }
}

export {
    img_avatar_UnknownAvatar,
    img_ui_background_root_cluster,
} 
