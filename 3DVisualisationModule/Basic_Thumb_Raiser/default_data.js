import * as THREE from "three";
import Orientation from "./orientation.js";

export const generalData = {
    setDevicePixelRatio: false
}

export const mazeParameters={ scale: new THREE.Vector3(1.0, 0.5, 1.0) };

export const buildingA1Data = {
    url: "./mazes/EdA-P1.json",
    credits: "Maze designed by Cecília Fernandes and Nikita.",
    scale: new THREE.Vector3(1.0, 1.0, 1.0),
}

export const hospitalBedPositions = {
    url : "./mazes/hospital_beds_positions.json",
}

export const patientPresence = {
    url : "./mazes/patient_presence.json",
}

export const playerData = {
    //url: "./models/gltf/NewRobot/animated_humanoid_robot.glb",
    //credits: "Model and related code snippets created by <a href='https://www.patreon.com/quaternius' target='_blank' rel='noopener'>Tomás Laulhé</a>. CC0 1.0. Modified by <a href='https://donmccurdy.com/' target='_blank' rel='noopener'>Don McCurdy</a>.",

    url: "./models/gltf/NewRobot/grandpa.glb",
    credits: "Model and related code snippets created by <a href='https://sketchfab.com/3d-models/grandpa-83d8d29ae74e4eb89775c9c0c7bc5e45",
    eyeHeight: 0.3, // fraction of character height
    scale: new THREE.Vector3(0.5, 0.4, 0.5),
    walkingSpeed:2,
    initialDirection: 25.3, // Expressed in degrees
    turningSpeed: 180.0, // Expressed in degrees / second
    runningFactor: 2.0, // Affects walking speed and turning speed
    keyCodes: { fixedView: "Digit1", firstPersonView: "Digit2", thirdPersonView: "Digit3", topView: "Digit4", viewMode: "KeyV", userInterface: "KeyU", miniMap: "KeyM", help: "KeyH", statistics: "KeyS", run: "KeyR", left: "ArrowLeft", right: "ArrowRight", backward: "ArrowDown", forward: "ArrowUp" }
}


export const doorData = {
    url: "./models/gltf/Door/animated_door.glb",
    credits: "https://sketchfab.com/3d-models/animated-low-poly-door-b31949b739874c119d31d89a3ec942a3",
    scale: new THREE.Vector3(0.40, 0.20, 0.40)
    //open:2,
    //close:2
}

export const lightsData = {
    ambientLight: { color: 0xffffff, intensity: 1.0 },
    pointLight1: { color: 0xffffff, intensity: 1.0, distance: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0) },
    pointLight2: { color: 0xffffff, intensity: 1.0, distance: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0) },
    spotLight: { color: 0xffffff, intensity: 1.0, distance: 0.0, angle: Math.PI / 3.0, penumbra: 0.0, position: new THREE.Vector3(0.0, 0.0, 0.0), direction: 0.0 } // angle and direction expressed in radians
}

export const fogData = {
    enabled: false,
    color: 0xe0e0e0,
    near: 0.1,
    far: 14.0
}

export const hospitalBedData = {
    url : "./models/gltf/HospitalBed/hospital_bed_purple.glb",
    credits : "https://sketchfab.com/3d-models/simple-hospital-bed-60651eac7e96454c8cf5c43a91412336",
    scale : new THREE.Vector3(0.025, 0.04, 0.03)
}

export const patientData = {
    url : "./models/gltf/Patient/patient.glb",
    credits : "https://sketchfab.com/3d-models/patient-00b483f284a542899b94e99831f1ad1c",
    scale : new THREE.Vector3(0.005, 0.005, 0.005)
}



export const cameraData = {
    view: "fixed", // Fixed view: "fixed"; first-person view: "first-person"; third-person view: "third-person"; top view: "top"; mini-map: "mini-map"
    multipleViewsViewport: new THREE.Vector4(0.0, 0.0, 1.0, 1.0), // Viewport position and size: fraction of window width and window height; MUST BE REDEFINED when creating an instance of ThumbRaiser() so that each view is assigned a different viewport
    target: new THREE.Vector3(0.0, 0.0, 0.0), // Target position
    initialOrientation: new Orientation(135.0, -45.0), // Horizontal and vertical orientation and associated limits (expressed in degrees)
    orientationMin: new Orientation(-180.0, -90.0),
    orientationMax: new Orientation(180.0, 0.0),
    initialDistance: 8.0, // Distance to the target and associated limits
    distanceMin: 4.0,
    distanceMax: 16.0,
    initialZoom: 1.0, // Zoom factor and associated limits
    zoomMin: 0.5,
    zoomMax: 2.0,
    initialFov: 45.0, // Field-of-view (expressed in degrees)
    near: 0.01, // Front clipping plane
    far: 100.0 // Back clipping plane
}