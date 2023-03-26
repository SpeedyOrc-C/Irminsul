"use strict";

let clusterShowcase = document.getElementById('cluster-showcase')

let viewX = 0
let viewY = 0
let viewScaleExponent = 0;

document.addEventListener('keydown', e => {
    let previousScaleExponent = viewScaleExponent
    let newTransform = ''

    switch (e.code) {
        case 'KeyW':
            viewY += 10
            break
        case 'KeyS':
            viewY -= 10
            break
        case 'KeyA':
            viewX += 10
            break
        case 'KeyD':
            viewX -= 10
            break
        case 'Minus':
            viewScaleExponent -= 1
            break
        case 'Equal':
            viewScaleExponent += 1
            break
        case 'Digit0':
            viewX = 0
            viewY = 0
            viewScaleExponent = 0
            newTransform = 'translate(0rem, 0rem) scale(100%)'
            break
    }

    let ratioThisScaleAndPreviousScale =
        Math.pow(2, 0.5 * (viewScaleExponent - previousScaleExponent))
    viewX *= ratioThisScaleAndPreviousScale
    viewY *= ratioThisScaleAndPreviousScale
    let viewScale = Math.pow(2, 0.5 * viewScaleExponent)

    newTransform = `translate(${viewX}rem, ${viewY}rem) scale(${viewScale*100}%)`
    clusterShowcase.style.transform = newTransform
})


