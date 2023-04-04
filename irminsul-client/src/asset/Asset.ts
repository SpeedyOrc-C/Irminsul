import img_ui_background_root_cluster from './img/ui/background-root-cluster.png'
import img_avatar_UnknownAvatar from './img/avatar/_UnknownAvatar.png'

export const asset: Record<string, () => Promise<any>> = import.meta.glob('./**/*')

export function getImgAvatar(id: string, assign: (result: string) => void) {
    asset[`./img/avatar/${id}.png`]().then(result => assign(result.default))
}

export {
    img_avatar_UnknownAvatar,
    img_ui_background_root_cluster,
} 
