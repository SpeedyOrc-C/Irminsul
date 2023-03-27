"use strict"

let clusterShowcase = document.getElementById('cluster-showcase')

// View Controller

let viewX = 0
let viewY = 0
let viewAngle = 0
let viewScaleExponent = 0
let viewScale = 1

function updateViewScale() {
    viewScale = Math.pow(2, 0.5 * viewScaleExponent)
}

function updateTransform() {
    clusterShowcase.style.transform =
        `rotate(${-viewAngle}deg)` +
        `scale(${viewScale*100}%)` +
        `translate(${viewX}rem, ${-viewY}rem)` +
        ''
}

function moveUp() {
    let deltaViewX = 0
    let deltaViewY = 0
    let viewAngleRad = viewAngle * Math.PI / 180

    deltaViewX -= 10 / viewScale * Math.sin(viewAngleRad)
    deltaViewY -= 10 / viewScale * Math.cos(viewAngleRad)

    viewAngle %= 360
    viewX += deltaViewX
    viewY += deltaViewY
    updateTransform()
}

function moveDown() {
    let viewAngleRad = viewAngle * Math.PI / 180
    viewX += 10 / viewScale * Math.sin(viewAngleRad)
    viewY += 10 / viewScale * Math.cos(viewAngleRad)
    updateTransform()
}

function moveLeft() {
    let viewAngleRad = viewAngle * Math.PI / 180
    viewX += 10 / viewScale * Math.cos(viewAngleRad)
    viewY -= 10 / viewScale * Math.sin(viewAngleRad)
    updateTransform()
}

function moveRight() {
    let viewAngleRad = viewAngle * Math.PI / 180
    viewX -= 10 / viewScale * Math.cos(viewAngleRad)
    viewY += 10 / viewScale * Math.sin(viewAngleRad)
    updateTransform()
}

function zoomIn() {
    viewScaleExponent += 1
    updateViewScale()
    updateTransform()
}

function zoomOut() {
    viewScaleExponent -= 1
    updateViewScale()
    updateTransform()
}

function rotateAnticlockwise() {
    viewAngle += 22.5
    updateTransform()
}

function rotateClockwise() {
    viewAngle -= 22.5
    updateTransform()
}

function resetView() {
    viewX = 0
    viewY = 0
    viewScaleExponent = 0
    viewScale = 1
    viewAngle = 0
    updateTransform()
}

// For keyboard users

document.addEventListener('keydown', e => {
    switch (e.code) {
        case 'KeyW': moveUp(); break
        case 'KeyS': moveDown(); break
        case 'KeyA': moveLeft(); break
        case 'KeyD': moveRight(); break
        case 'Minus': zoomOut(); break
        case 'Equal': zoomIn(); break
        case 'BracketLeft': rotateClockwise(); break
        case 'BracketRight': rotateAnticlockwise(); break
        case 'Digit0': resetView(); break
    }
})

// For touchscreen users

let touchscreenControls = document.createElement('div')
document.body.append(touchscreenControls)
touchscreenControls.id = 'touchscreen-control'




