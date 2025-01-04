// Thumb Raiser - JPP 2021, 2022, 2023
// 3D modeling
// 3D models importing
// Perspective and orthographic projections
// Viewing
// Linear and affine transformations
// Lighting and materials
// Shadow projection
// Fog
// Texture mapping
// User interaction

import * as THREE from "three";
import Stats from "three/addons/libs/stats.module.js";
import Orientation from "./orientation.js";
import { generalData, buildingA1Data, playerData, lightsData, fogData, cameraData, doorData, hospitalBedData, patientData, hospitalBedPositions, patientPresence } from "./default_data.js";
import { merge } from "./merge.js";
import Maze from "./maze.js";
import Player from "./player.js";
import Door from "./door.js";
import HospitalBed from "./hospital_bed.js";
import Lights from "./lights.js";
import Fog from "./fog.js";
import Camera from "./camera.js";
import UserInterface from "./user_interface.js";
import Animations from "./animations.js";
import Animations_door from "./animations_door.js";
import Patient from "./patient.js";
import ChandelierLight from "./chandelier_light.js";


export default class ThumbRaiser {
    constructor(generalParameters, mazeParameters, buildingParameters, playerParameters, lightsParameters, fogParameters, fixedViewCameraParameters, firstPersonViewCameraParameters, thirdPersonViewCameraParameters, topViewCameraParameters, miniMapCameraParameters, doorParameters) {

        this.generalParameters = merge({}, generalData, generalParameters);

        this.buildingA1Parameters = merge({}, buildingA1Data, mazeParameters);
        this.hospitalBedParameters = hospitalBedData;
        this.patientParameters = patientData;



        this.playerParameters = merge({}, playerData, playerParameters);
        this.lightsParameters = merge({}, lightsData, lightsParameters);
        this.fogParameters = merge({}, fogData, fogParameters);
        this.fixedViewCameraParameters = merge({}, cameraData, fixedViewCameraParameters);
        this.firstPersonViewCameraParameters = merge({}, cameraData, firstPersonViewCameraParameters);
        this.thirdPersonViewCameraParameters = merge({}, cameraData, thirdPersonViewCameraParameters);
        this.topViewCameraParameters = merge({}, cameraData, topViewCameraParameters);
        this.miniMapCameraParameters = merge({}, cameraData, miniMapCameraParameters);
        this.doorParameters = merge({}, doorData, doorParameters);

        // Create a 2D scene (the viewports frames)
        this.scene2D = new THREE.Scene();

        // Create a square
        let points = [new THREE.Vector3(0.0, 0.0, 0.0), new THREE.Vector3(1.0, 0.0, 0.0), new THREE.Vector3(1.0, 1.0, 0.0), new THREE.Vector3(0.0, 1.0, 0.0)];
        let geometry = new THREE.BufferGeometry().setFromPoints(points);
        const material = new THREE.LineBasicMaterial({ color: 0xffffff });
        this.square = new THREE.LineLoop(geometry, material);
        this.scene2D.add(this.square);

        // Create the camera corresponding to the 2D scene
        this.camera2D = new THREE.OrthographicCamera(0.0, 1.0, 1.0, 0.0, 0.0, 1.0);

        // Create a 3D scene (the game itself)
        this.scene3D = new THREE.Scene();

        // Create the maze
        this.maze = new Maze(buildingParameters, this.doorParameters, 0);

        // Create the hospital bed
        this.hospitalBed = new HospitalBed(this.hospitalBedParameters);

        // Create the patient
        this.patient = new Patient(this.patientParameters);

        // Create the player
        this.player = new Player(this.playerParameters);

        // Create the lights
        this.lights = new Lights(this.lightsParameters);

        this.chandelierLights = new Map();
        this.currentRoomNumberTarget = 0;

        // Create the fog
        this.fog = new Fog(this.fogParameters);

        // Create the cameras corresponding to the four different views: fixed view, first-person view, third-person view and top view
        this.fixedViewCamera = new Camera(this.fixedViewCameraParameters, window.innerWidth, window.innerHeight);
        this.firstPersonViewCamera = new Camera(this.firstPersonViewCameraParameters, window.innerWidth, window.innerHeight);
        this.thirdPersonViewCamera = new Camera(this.thirdPersonViewCameraParameters, window.innerWidth, window.innerHeight);
        this.topViewCamera = new Camera(this.topViewCameraParameters, window.innerWidth, window.innerHeight);

        // Create the mini-map camera
        this.miniMapCamera = new Camera(this.miniMapCameraParameters, window.innerWidth, window.innerHeight);

        // Create the statistics and make its node invisible
        this.statistics = new Stats();
        this.statistics.dom.style.visibility = "hidden";
        document.body.appendChild(this.statistics.dom);

        //Create the Automatic Movement Table and make its node invisible


        this.container = document.createElement('container');

        this.buttonContainer = document.createElement('matrix');

        //For storing the selected room number
        this.selectedRoomNumber = null;

        //document.body.appendChild(this.buttonContainer);



        // this.buttonMatrixList = this.container.querySelectorAll('buttonMatrix');





        // Create a renderer and turn on shadows in the renderer
        this.renderer = new THREE.WebGLRenderer({ antialias: true });
        if (this.generalParameters.setDevicePixelRatio) {
            this.renderer.setPixelRatio(window.devicePixelRatio);
        }
        this.renderer.autoClear = false;
        this.renderer.shadowMap.enabled = true;
        this.renderer.shadowMap.type = THREE.PCFSoftShadowMap;
        this.renderer.setSize(window.innerWidth, window.innerHeight);
        //document.body.appendChild(this.renderer.domElement);

        this.container.appendChild(this.renderer.domElement);
        document.body.appendChild(this.container);
        // Set the mouse move action (none)
        this.dragMiniMap = false;
        this.changeCameraDistance = false;
        this.changeCameraOrientation = false;

        this.container.appendChild(this.buttonContainer);

        // Set the game state
        this.gameRunning = false;

        // Get and configure the panel's <div> elements
        this.viewsPanel = document.getElementById("views-panel");
        this.view = document.getElementById("view");
        this.projection = document.getElementById("projection");
        this.horizontal = document.getElementById("horizontal");
        this.horizontal.step = 1;
        this.vertical = document.getElementById("vertical");
        this.vertical.step = 1;
        this.distance = document.getElementById("distance");
        this.distance.step = 0.1;
        this.zoom = document.getElementById("zoom");
        this.zoom.step = 0.1;
        this.reset = document.getElementById("reset");
        this.resetAll = document.getElementById("reset-all");
        this.helpPanel = document.getElementById("help-panel");
        this.helpPanel.style.visibility = "hidden";
        this.subwindowsPanel = document.getElementById("subwindows-panel");
        this.multipleViewsCheckBox = document.getElementById("multiple-views");
        this.multipleViewsCheckBox.checked = false;
        this.userInterfaceCheckBox = document.getElementById("user-interface");
        this.userInterfaceCheckBox.checked = true;
        this.miniMapCheckBox = document.getElementById("mini-map");
        this.miniMapCheckBox.checked = true;
        this.helpCheckBox = document.getElementById("help");
        this.helpCheckBox.checked = false;
        this.statisticsCheckBox = document.getElementById("statistics");
        this.statisticsCheckBox.checked = false;


        // Build the help panel
        this.buildHelpPanel();

        // Set the active view camera (fixed view)
        this.setActiveViewCamera(this.fixedViewCamera);

        // Arrange viewports by view mode
        this.arrangeViewports(this.multipleViewsCheckBox.checked);

        // Register the event handler to be called on window resize
        window.addEventListener("resize", event => this.windowResize(event));

        // Register the event handler to be called on key down
        document.addEventListener("keydown", event => this.keyChange(event, true));

        // Register the event handler to be called on key release
        document.addEventListener("keyup", event => this.keyChange(event, false));

        this.raycaster = new THREE.Raycaster();
        // Register the event handler to be called on a click on a bed
        window.addEventListener("click", event => this.centerSelectedBed(event))

        // Register the event handler to be called on mouse down
        this.renderer.domElement.addEventListener("mousedown", event => this.mouseDown(event));

        // Register the event handler to be called on mouse move
        this.renderer.domElement.addEventListener("mousemove", event => this.mouseMove(event));

        // Register the event handler to be called on mouse up
        this.renderer.domElement.addEventListener("mouseup", event => this.mouseUp(event));

        // Register the event handler to be called on mouse wheel
        this.renderer.domElement.addEventListener("wheel", event => this.mouseWheel(event));

        // Register the event handler to be called on context menu
        this.renderer.domElement.addEventListener("contextmenu", event => this.contextMenu(event));

        // Register the event handler to be called on select, input number, or input checkbox change
        this.view.addEventListener("change", event => this.elementChange(event));
        this.projection.addEventListener("change", event => this.elementChange(event));
        this.horizontal.addEventListener("change", event => this.elementChange(event));
        this.vertical.addEventListener("change", event => this.elementChange(event));
        this.distance.addEventListener("change", event => this.elementChange(event));
        this.zoom.addEventListener("change", event => this.elementChange(event));
        this.multipleViewsCheckBox.addEventListener("change", event => this.elementChange(event));
        this.userInterfaceCheckBox.addEventListener("change", event => this.elementChange(event));
        this.helpCheckBox.addEventListener("change", event => this.elementChange(event));
        this.statisticsCheckBox.addEventListener("change", event => this.elementChange(event));

        // Register the event handler to be called on input button click
        this.reset.addEventListener("click", event => this.buttonClick(event));
        this.resetAll.addEventListener("click", event => this.buttonClick(event));

        this.activeElement = document.activeElement;


        // Add event listener for the 'i' key to toggle room information
        document.addEventListener('keydown', event => this.toggleRoomInfo(event, patientPresence.url));

    }



    movePlayerToPosition(i, j) {
        i = i - 5;
        j = j - 11;

        const deltaT = this.clock.getDelta();

        const destination = new THREE.Vector3(j, 0, i);

        // Calculate the direction and distance to the destination
        const direction = destination.clone().sub(this.player.position);
        const distance = direction.length();


        // while(distance >= 0.1){
        // Set the player's direction
        this.player.direction = Math.atan2(direction.x, direction.z) * (180 / Math.PI);

        // Move the player towards the destination
        const speed = 5.0; // Adjust the speed as needed
        const coveredDistance = Math.min(speed * deltaT, distance);
        const movement = direction.clone().normalize().multiplyScalar(coveredDistance);
        this.player.position.add(movement);

        // Update the player's object position
        this.player.object.position.copy(this.player.position);

        // Check if the player has reached the destination
        if (distance < 0.1) {
            // Player has reached the destination
            console.log('Player reached destination');
        }
        //}

    }

    /*  while(this.player.position != newPosition){
                if (this.player.position.x > newPosition.x ) this.player.position.x=this.player.position.x+0.1;
                else if (this.player.position.x < newPosition.x) this.player.position.x=this.player.position.x-0.1;

                if(this.player.position.z > newPosition.z )this.player.position.z=this.player.position.z+0.1
                else if (this.player.position.z < newPosition.z) this.player.position.z=this.player.position.z-0.1;
            } */

    buildHelpPanel() {
        const table = document.getElementById("help-table");
        let i = 0;
        for (const key in this.player.keyCodes) {
            while (table.rows[i].cells.length < 2) {
                i++;
            };
            table.rows[i++].cells[0].innerHTML = this.player.keyCodes[key];
        }
        table.rows[i].cells[0].innerHTML = this.maze.credits + "<br>" + this.player.credits;
        table

    }

    displayPanel() {
        this.view.options.selectedIndex = ["fixed", "first-person", "third-person", "top"].indexOf(this.activeViewCamera.view);
        this.projection.options.selectedIndex = ["perspective", "orthographic"].indexOf(this.activeViewCamera.projection);
        this.horizontal.value = this.activeViewCamera.orientation.h.toFixed(0);
        this.vertical.value = this.activeViewCamera.orientation.v.toFixed(0);
        this.distance.value = this.activeViewCamera.distance.toFixed(1);
        this.zoom.value = this.activeViewCamera.zoom.toFixed(1);

    }

    // Set active view camera
    setActiveViewCamera(camera) {
        this.activeViewCamera = camera;
        this.horizontal.min = this.activeViewCamera.orientationMin.h.toFixed(0);
        this.horizontal.max = this.activeViewCamera.orientationMax.h.toFixed(0);
        this.vertical.min = this.activeViewCamera.orientationMin.v.toFixed(0);
        this.vertical.max = this.activeViewCamera.orientationMax.v.toFixed(0);
        this.distance.min = this.activeViewCamera.distanceMin.toFixed(1);
        this.distance.max = this.activeViewCamera.distanceMax.toFixed(1);
        this.zoom.min = this.activeViewCamera.zoomMin.toFixed(1);
        this.zoom.max = this.activeViewCamera.zoomMax.toFixed(1);
        this.displayPanel();

    }

    arrangeViewports(multipleViews) {
        this.fixedViewCamera.setViewport(multipleViews);
        this.firstPersonViewCamera.setViewport(multipleViews);
        this.thirdPersonViewCamera.setViewport(multipleViews);
        this.topViewCamera.setViewport(multipleViews);
    }

    pointerIsOverViewport(pointer, viewport) {
        return (
            pointer.x >= viewport.x &&
            pointer.x < viewport.x + viewport.width &&
            pointer.y >= viewport.y &&
            pointer.y < viewport.y + viewport.height);
    }

    getPointedViewport(pointer) {
        let viewport;
        // Check if the pointer is over the mini-map camera viewport
        if (this.miniMapCheckBox.checked) {
            viewport = this.miniMapCamera.getViewport();
            if (this.pointerIsOverViewport(pointer, viewport)) {
                return this.miniMapCamera.view;
            }
        }
        // Check if the pointer is over the remaining camera viewports
        let cameras;
        if (this.multipleViewsCheckBox.checked) {
            cameras = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera];
        }
        else {
            cameras = [this.activeViewCamera];
        }
        for (const camera of cameras) {
            viewport = camera.getViewport();
            if (this.pointerIsOverViewport(pointer, viewport)) {
                return camera.view;
            }
        }
        // No camera viewport is being pointed
        return "none";
    }

    setViewMode(multipleViews) { // Single-view mode: false; multiple-views mode: true
        this.multipleViewsCheckBox.checked = multipleViews;
        this.arrangeViewports(this.multipleViewsCheckBox.checked);
    }

    setUserInterfaceVisibility(visible) {
        this.userInterfaceCheckBox.checked = visible;
        this.viewsPanel.style.visibility = visible ? "visible" : "hidden";
        this.subwindowsPanel.style.visibility = visible ? "visible" : "hidden";
        this.userInterface.setVisibility(visible);
    }

    setMiniMapVisibility(visible) { // Hidden: false; visible: true
        this.miniMapCheckBox.checked = visible;
    }

    setHelpVisibility(visible) { // Hidden: false; visible: true
        this.helpCheckBox.checked = visible;
        this.helpPanel.style.visibility = visible ? "visible" : "hidden";
    }

    setStatisticsVisibility(visible) { // Hidden: false; visible: true
        this.statisticsCheckBox.checked = visible;
        this.statistics.dom.style.visibility = visible ? "visible" : "hidden";
    }

    windowResize() {
        this.fixedViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.firstPersonViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.thirdPersonViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.topViewCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.miniMapCamera.updateWindowSize(window.innerWidth, window.innerHeight);
        this.renderer.setSize(window.innerWidth, window.innerHeight);
    }

     // Method to handle key press for showing/hiding room info
     toggleRoomInfo(event, patientsUrl) {
        if (event.code === "KeyI") {
            const overlay = document.getElementById('room-info-overlay');
            const title = document.getElementById('room-info-title');
            const content = document.getElementById('room-info-content');

            if (this.selectedRoomNumber) {
                // Fetch the data for the selected room
                fetch(patientsUrl)
                    .then(response => response.json())
                    .then(data => {
                        // Find the room that matches the selected room number
                        const selectedRoom = data.Surgery_Rooms.find(
                            room => room["Room Number"] === this.selectedRoomNumber
                        );

    
                        if (selectedRoom) {
                            const isBeingUsed = selectedRoom.IsBeingUsed;
                            let message;

                            if(isBeingUsed){
                            
                                const randomDuration = Math.floor(Math.random() * 180) + 1;

                                // Split the duration into two random parts
                                const randomSplit = Math.random(); // Fraction between 0 and 1
                                const part1 = Math.floor(randomDuration * randomSplit); // First part (past)
                                const part2 = randomDuration - part1; // Second part (future)
    
                                const currentTime = new Date();
    
                                // Calculate the start and end times
                                const startTime = new Date(
                                    currentTime.getTime() - part1 * 60 * 1000
                                );
                                const endTime = new Date(
                                    currentTime.getTime() + part2 * 60 * 1000
                                );
                            
                                const formatTime = time =>
                                    time.toTimeString().split(' ')[0].substring(0, 5);

                                message = `Room ${this.selectedRoomNumber} is currently in use.\nCurrent surgery time: ${formatTime(
                                    startTime
                                )} - ${formatTime(endTime)}.`;
                            
                            
                            } else {
                                message = `Room ${this.selectedRoomNumber} is not in use.`;
                            }

                            // Populate the overlay with the information
                            title.textContent = `Room ${this.selectedRoomNumber}`;
                            content.textContent = message;
    
                            // Toggle overlay visibility
                            overlay.style.display =
                                overlay.style.display === "none" ? "block" : "none";
                        } else {
                            console.warn("Room data not found for selected room.");
                        }
                    })
                    .catch(error => console.error("Error loading JSON:", error));
            } else {
                console.warn("No room selected to display information.");
            }
        }
    }


    centerSelectedBed(event) {
        // Convert mouse position to normalized device coordinates (-1 to +1)
        this.mousePosition.x = (event.clientX / window.innerWidth) * 2 - 1;
        this.mousePosition.y = -(event.clientY / window.innerHeight) * 2 + 1;

        // Update raycaster with camera and mouse position
        this.raycaster.setFromCamera(this.mousePosition, this.activeViewCamera.getActiveProjection());

        // Perform raycasting
        const intersects = this.raycaster.intersectObjects(this.scene3D.children, true);
        if (intersects.length > 0) {
            const pickedObject = intersects[0].object?.parent?.parent?.parent?.parent?.parent;

            //console.log(pickedObject.userData);
            if (pickedObject?.userData?.selectable) {
                const objectPosition = pickedObject.position;
                this.selectedRoomNumber = pickedObject.number;

                console.log("Selected " + pickedObject.name + " in position "
                    + "{ x:" + objectPosition.x
                    + ", y:" + objectPosition.y
                    + ", z:" + objectPosition.z + " }");
                // Define the new camera position
                const offset = new THREE.Vector3(0,6,0);
                const newPosition = objectPosition.clone().add(offset);

                // Current camera position
                const camera = this.activeViewCamera.getActiveProjection();
                const currentPosition = camera.position.clone();

                if(pickedObject.number !== this.currentRoomNumberTarget) {

                    //Current camera target
                    const currentTarget = new THREE.Vector3();
                    camera.getWorldDirection(currentTarget).add(camera.position);

                    //the duration of the animations have to be the same so it is synched
                    //the duration is calculated based on the distance to ensure farther objects are not too fast to center
                    const distance = currentPosition.distanceTo(newPosition);
                    const duration = Math.min(4000, 500 * distance);

                    //The Tween for the position and target had to be separate so a glitching move does not happen.
                    // Tween the camera position
                    new TWEEN.Tween(currentPosition)
                        .to(newPosition, duration) // Transition duration in milliseconds
                        .easing(TWEEN.Easing.Cubic.InOut) // Easing function
                        .onUpdate(() => {
                            camera.position.set(currentPosition.x, currentPosition.y, currentPosition.z);
                        })
                        .start();

                    //Tween camera lookAt target
                    new TWEEN.Tween(currentTarget)
                        .to(objectPosition, duration)
                        .easing(TWEEN.Easing.Cubic.InOut)
                        .onUpdate(() => {
                            camera.lookAt(currentTarget);
                        })
                        .start();

                    if(this.currentRoomNumberTarget!==0) {
                        this.chandelierLights.get(this.currentRoomNumberTarget).hideLight(duration);
                    }
                    let light = this.chandelierLights.get(pickedObject.number);
                    light.showLight(duration);
                    this.currentRoomNumberTarget=pickedObject.number;
                }
            }
        }
    }

    keyChange(event, state) {
        // Allow digit and arrow keys to be used when entering numbers
        if (["horizontal", "vertical", "distance", "zoom"].indexOf(event.target.id) < 0) {
            event.target.blur();
        }
        if (document.activeElement == document.body) {
            // Prevent the "Space" and "Arrow" keys from scrolling the document's content
            if (event.code == "Space" || event.code == "ArrowLeft" || event.code == "ArrowRight" || event.code == "ArrowDown" || event.code == "ArrowUp") {
                event.preventDefault();
            }
            if (event.code == this.player.keyCodes.fixedView && state) { // Select fixed view
                this.setActiveViewCamera(this.fixedViewCamera);
            }
            else if (event.code == this.player.keyCodes.firstPersonView && state) { // Select first-person view
                this.setActiveViewCamera(this.firstPersonViewCamera);
            }
            else if (event.code == this.player.keyCodes.thirdPersonView && state) { // Select third-person view
                this.setActiveViewCamera(this.thirdPersonViewCamera);
            }
            else if (event.code == this.player.keyCodes.topView && state) { // Select top view
                this.setActiveViewCamera(this.topViewCamera);
            }
            if (event.code == this.player.keyCodes.viewMode && state) { // Single-view mode / multiple-views mode
                this.setViewMode(!this.multipleViewsCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.userInterface && state) { // Display / hide user interface
                this.setUserInterfaceVisibility(!this.userInterfaceCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.miniMap && state) { // Display / hide mini-map
                this.setMiniMapVisibility(!this.miniMapCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.help && state) { // Display / hide help
                this.setHelpVisibility(!this.helpCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.statistics && state) { // Display / hide statistics
                this.setStatisticsVisibility(!this.statisticsCheckBox.checked);
            }
            if (event.code == this.player.keyCodes.run) {
                this.player.keyStates.run = state;
            }
            if (event.code == this.player.keyCodes.left) {
                this.player.keyStates.left = state;
            }
            else if (event.code == this.player.keyCodes.right) {
                this.player.keyStates.right = state;
            }
            if (event.code == this.player.keyCodes.backward) {
                this.player.keyStates.backward = state;
            }
            else if (event.code == this.player.keyCodes.forward) {
                this.player.keyStates.forward = state;
            }
            if (event.code == this.player.keyCodes.dance) {
                this.player.keyStates.dance = state;
            }

        }
    }

    mouseDown(event) {
        if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
            // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
            this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
            // Select the camera whose view is being pointed
            const cameraView = this.getPointedViewport(this.mousePosition);
            if (cameraView != "none") {
                if (cameraView == "mini-map") { // Mini-map camera selected
                    if (event.buttons == 1) { // Primary button down
                        this.dragMiniMap = true;
                    }
                }
                else { // One of the remaining cameras selected
                    const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
                    this.view.options.selectedIndex = cameraIndex;
                    this.setActiveViewCamera([this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][cameraIndex]);
                    if (event.buttons == 1) { // Primary button down
                        this.changeCameraDistance = true;
                    }
                    else { // Secondary button down
                        this.changeCameraOrientation = true;
                    }
                }
            }
        }
    }

    mouseMove(event) {
        if (event.buttons == 1 || event.buttons == 2) { // Primary or secondary button down
            if (this.changeCameraDistance || this.changeCameraOrientation || this.dragMiniMap) { // Mouse action in progress
                // Compute mouse movement and update mouse position
                const newMousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
                const mouseIncrement = newMousePosition.clone().sub(this.mousePosition);
                this.mousePosition = newMousePosition;
                if (event.buttons == 1) { // Primary button down
                    if (this.changeCameraDistance) {
                        this.activeViewCamera.updateDistance(-0.05 * (mouseIncrement.x + mouseIncrement.y));
                        this.displayPanel();
                    }
                    else if (this.dragMiniMap) {
                        const windowMinSize = Math.min(window.innerWidth, window.innerHeight);
                        const width = this.miniMapCamera.viewport.width * windowMinSize;
                        const height = this.miniMapCamera.viewport.height * windowMinSize;
                        this.miniMapCamera.viewport.x += mouseIncrement.x / (window.innerWidth - width);
                        this.miniMapCamera.viewport.y += mouseIncrement.y / (window.innerHeight - height);
                    }
                }
                else { // Secondary button down
                    if (this.changeCameraOrientation) {
                        this.activeViewCamera.updateOrientation(mouseIncrement.multiply(new THREE.Vector2(-0.5, 0.5)));
                        this.displayPanel();
                    }
                }
            }
        }
    }

    mouseUp(event) {
        // Reset mouse move action
        this.dragMiniMap = false;
        this.changeCameraDistance = false;
        this.changeCameraOrientation = false;
    }

    mouseWheel(event) {
        // Prevent the mouse wheel from scrolling the document's content
        event.preventDefault();
        // Store current mouse position in window coordinates (mouse coordinate system: origin in the top-left corner; window coordinate system: origin in the bottom-left corner)
        this.mousePosition = new THREE.Vector2(event.clientX, window.innerHeight - event.clientY - 1);
        // Select the camera whose view is being pointed
        const cameraView = this.getPointedViewport(this.mousePosition);
        if (cameraView != "none" && cameraView != "mini-map") { // One of the remaining cameras selected
            const cameraIndex = ["fixed", "first-person", "third-person", "top"].indexOf(cameraView);
            this.view.options.selectedIndex = cameraIndex;
            const activeViewCamera = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][cameraIndex];
            activeViewCamera.updateZoom(-0.001 * event.deltaY);
            this.setActiveViewCamera(activeViewCamera);
        }
    }

    contextMenu(event) {
        // Prevent the context menu from appearing when the secondary mouse button is clicked
        event.preventDefault();
    }

    elementChange(event) {
        switch (event.target.id) {
            case "view":
                this.setActiveViewCamera([this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera][this.view.options.selectedIndex]);
                break;
            case "projection":
                this.activeViewCamera.setActiveProjection(["perspective", "orthographic"][this.projection.options.selectedIndex]);
                this.displayPanel();
                break;
            case "horizontal":
            case "vertical":
            case "distance":
            case "zoom":
                if (event.target.checkValidity()) {
                    switch (event.target.id) {
                        case "horizontal":
                        case "vertical":
                            this.activeViewCamera.setOrientation(new Orientation(this.horizontal.value, this.vertical.value));
                            break;
                        case "distance":
                            this.activeViewCamera.setDistance(this.distance.value);
                            break;
                        case "zoom":
                            this.activeViewCamera.setZoom(this.zoom.value);
                            break;
                    }
                }
                break;
            case "multiple-views":
                this.setViewMode(event.target.checked);
                break;
            case "user-interface":
                this.setUserInterfaceVisibility(event.target.checked);
                break;
            case "help":
                this.setHelpVisibility(event.target.checked);
                break;
            case "statistics":
                this.setStatisticsVisibility(event.target.checked);
                break;
        }
    }

    buttonClick(event) {
        switch (event.target.id) {
            case "reset":
                this.activeViewCamera.initialize();
                break;
            case "reset-all":
                this.fixedViewCamera.initialize();
                this.firstPersonViewCamera.initialize();
                this.thirdPersonViewCamera.initialize();
                this.topViewCamera.initialize();
                break;
        }
        this.displayPanel();
    }

    finalSequence() {
        // Reconfigure the third-person view camera
        this.thirdPersonViewCamera.setOrientation(new Orientation(180.0, this.thirdPersonViewCamera.initialOrientation.v));
        this.thirdPersonViewCamera.setDistance(this.thirdPersonViewCamera.initialDistance);
        this.thirdPersonViewCamera.setZoom(2.0);
        // Set it as the active view camera
        this.setActiveViewCamera(this.thirdPersonViewCamera);
        // Set single-view mode
        this.setViewMode(false);
    }

    collision(position) {
        return this.maze.distanceToEastWall(position) < (this.player.radius / 2) || this.maze.distanceToWestWall(position) < (this.player.radius / 2) ||
            this.maze.distanceToSouthWall(position) < (this.player.radius / 2) || this.maze.distanceToNorthWall(position) < (this.player.radius / 2)
    }

    collision_door(position) {
        return this.maze.distanceToWestDoor(position) < (this.player.radius / 2) || this.maze.distanceToEastDoor(position) < (this.player.radius / 2)
            || this.maze.distanceToNorthDoor(position) < (this.player.radius / 2) || this.maze.distanceToSouthDoor(position) < (this.player.radius / 2);
    }

    setHospitalBedsAndPatients(hospitalBedsUrl, patientsUrl) {

        let hospitalBedObject;
        let chandelierLightObject;
        let patientObject;
        let patientPositionArray = new Array();
        let patientRotationArray = new Array();


        fetch(hospitalBedsUrl)
            .then(response => response.json())
            .then(data => {
                data.Surgery_Rooms.forEach(room => {
                    //console.log(`Room Number: ${room["Room Number"]}`);
                    //console.log(`Hospital Bed Position: x=${room.hospitalBedPosition[0]}, y=${room.hospitalBedPosition[1]}`);
                    //console.log(`Rotation: ${room.rotation}`);
                    //console.log('---------------------');
                    hospitalBedObject = this.hospitalBed.object.clone();
                    let hospitalBedPosition = this.maze.cellToCartesian(room.hospitalBedPosition);
                    this.scene3D.add(hospitalBedObject);
                    hospitalBedObject.position.set(hospitalBedPosition.x, hospitalBedPosition.y, hospitalBedPosition.z);
                    hospitalBedObject.rotation.y = room.rotation;
                    hospitalBedObject.name = `HospitalBed_${room["Room Number"]}`;
                    hospitalBedObject.number = room["Room Number"];

                    patientPositionArray.push(room.patientPosition);
                    patientRotationArray.push(room.patientRotation);
                    
                    // Create the chandelierLight
                    let chandelierLightObject = new ChandelierLight(this.lightsParameters, hospitalBedPosition.x, hospitalBedPosition.y + 0.8, hospitalBedPosition.z);
                    this.chandelierLights.set(room["Room Number"], chandelierLightObject);
                    this.scene3D.add(chandelierLightObject.object);

                });
            })
            .catch(error => console.error('Error loading JSON:', error));

        let index = 0;

        fetch(patientsUrl)
            .then(response => response.json())
            .then(data => {
                data.Surgery_Rooms.forEach(room => {

                    if (room.IsBeingUsed == true) {
                        patientObject = this.patient.object.clone();
                        let patientPosition = this.maze.cellToCartesianWithHeight(patientPositionArray[index]);
                        this.scene3D.add(patientObject);
                        patientObject.position.set(patientPosition.x, patientPosition.y, patientPosition.z);
                        patientObject.rotation.y = (patientRotationArray[index]);
                    }
                    index++;

                });
            })
            .catch(error => console.error('Error loading JSON:', error));




    }

    update() {
        if (!this.gameRunning) {

            if (this.maze.loaded && this.player.loaded) { // If all resources have been loaded
                // Add the maze, the player and the lights to the scene

                this.doorsTR = new Array();

                console.log("tr.doors");
                console.log(this.maze.doors);

                console.log("Currentmaze");
                console.log(this.maze);

                console.log("Map")
                console.log(this.maze.map);

                this.matriz = this.maze.map;

                this.scene3D.add(this.maze.object);

                let y = 0;

                this.doorsTR = this.maze.doors;

                if (this.maze.doorObjectVertical.length > 0) {
                    //5
                    for (let i = 0; i < this.maze.doorObjectVertical.length; i++) {
                        this.scene3D.add(this.doorsTR[y].object);
                        this.doorsTR[y].object.position.set(this.maze.doorObjectVertical[i].x, this.maze.doorObjectVertical[i].y, this.maze.doorObjectVertical[i].z);
                        this.doorsTR[y].object.rotation.y = Math.PI / 2.0;
                        y++;
                    }
                }

                if (this.maze.doorObjectHorizontal.length > 0) {
                    //4
                    for (let i = 0; i < this.maze.doorObjectHorizontal.length; i++) {
                        // Create the door
                        this.scene3D.add(this.doorsTR[y].object);
                        this.doorsTR[y].object.position.set(this.maze.doorObjectHorizontal[i].x, this.maze.doorObjectHorizontal[i].y, this.maze.doorObjectHorizontal[i].z);
                        y++;
                    }
                }


                this.scene3D.add(this.player.object);
                this.scene3D.add(this.lights.object);


                // Create the clock
                this.clock = new THREE.Clock();

                // Create model animations (states)
                this.animations = new Animations(this.player.object, this.player.animations);


                this.animationsDoors = new Array();
                for (let i = 0; i < this.doorsTR.length; i++) {
                    this.animationsDoors.push(new Animations_door(this.doorsTR[i].object, this.doorsTR[i].animations_door, this.doorsTR[i].location));
                }


                // Set the player's position and direction
                this.player.position = this.maze.initialPosition;
                this.player.direction = this.maze.initialDirection;


                // Setting the hospital beds
                this.setHospitalBedsAndPatients(hospitalBedPositions.url, patientPresence.url);


                if (this.userInterface == null) {
                    // Create the user interface
                    this.userInterface = new UserInterface(this.scene3D, this.renderer, this.lights, this.maze, this, this.animations);
                }

                // Start the game
                this.gameRunning = true;

            }

        }
        else {
            this.buildMaze();
        }
    }

    slideAlongWall(newPosition, currentPosition) {
        // Try to slide horizontally
        const horizontalSlide = new THREE.Vector3(newPosition.x, currentPosition.y, currentPosition.z);
        if (!this.collision(horizontalSlide) && !this.collision_door(horizontalSlide)) {
            return horizontalSlide;
        }

        // Try to slide vertically
        const verticalSlide = new THREE.Vector3(currentPosition.x, currentPosition.y, newPosition.z);
        if (!this.collision(verticalSlide) && !this.collision_door(verticalSlide)) {
            return verticalSlide;
        }

        // If neither sliding direction works, return current position
        return currentPosition;
    }

    buildMaze() {
        // Update the model animations
        const deltaT = this.clock.getDelta();
        this.animations.update(deltaT);



        for (let i = 0; i < this.animationsDoors.length; i++) {
            this.animationsDoors[i].update(deltaT);
        }

        // Update the player
        if (!this.animations.actionInProgress) {
            // Check if the player found the exit
            if (this.maze.foundExit(this.player.position)) {
                this.finalSequence();
            }

            let coveredDistance = this.player.walkingSpeed * deltaT;
            let directionIncrement = this.player.turningSpeed * deltaT;
            if (this.player.keyStates.run) {
                coveredDistance *= this.player.runningFactor;
                directionIncrement *= this.player.runningFactor;
            }
            if (this.player.keyStates.left) {
                this.player.direction += directionIncrement;
            }
            else if (this.player.keyStates.right) {
                this.player.direction -= directionIncrement;
            }
            const direction = THREE.MathUtils.degToRad(this.player.direction);
            if (this.player.keyStates.backward) {
                const newPosition = new THREE.Vector3(-coveredDistance * Math.sin(direction), 0.0, -coveredDistance * Math.cos(direction)).add(this.player.position);
                if (!this.collision(newPosition) && !this.collision_door(newPosition)) {
                    this.animations.fadeToAction("metarig|Walk", 0.2);
                    this.player.position = newPosition;
                } else {
                    // Try to slide along the wall
                    this.player.position = this.slideAlongWall(newPosition, this.player.position);
                }
            }
            else if (this.player.keyStates.forward) {
                const newPosition = new THREE.Vector3(coveredDistance * Math.sin(direction), 0.0, coveredDistance * Math.cos(direction)).add(this.player.position);

                if (!this.collision(newPosition) && !this.collision_door(newPosition)) {
                    this.animations.fadeToAction("metarig|Walk", 0.2);
                    this.player.position = newPosition;
                } else {
                    // Try to slide along the wall
                    this.player.position = this.slideAlongWall(newPosition, this.player.position);
                }
            }
            else if (this.player.keyStates.dance) {
                this.animations.fadeToAction("metarig|Dance1", 0.9);
            }
            else {
                this.animations.fadeToAction("metarig|Idle(HeavyBreathing)", 0.2);
            }
            this.player.object.position.set(this.player.position.x, this.player.position.y, this.player.position.z);
            this.player.object.rotation.y = direction - this.player.initialDirection;

        }

        // Update first-person, third-person and top view cameras parameters (player direction and target)
        this.firstPersonViewCamera.playerDirection = this.player.direction;
        this.thirdPersonViewCamera.playerDirection = this.player.direction;
        this.topViewCamera.playerDirection = this.player.direction;
        const target = new THREE.Vector3(this.player.position.x, this.player.position.y + this.player.eyeHeight, this.player.position.z);
        this.firstPersonViewCamera.setTarget(target);
        this.thirdPersonViewCamera.setTarget(target);
        this.topViewCamera.setTarget(target);

        // Update statistics
        this.statistics.update();

        // Render primary viewport(s)
        this.renderer.clear();

        if (this.fog.enabled) {
            this.scene3D.fog = this.fog.object;
        }
        else {
            this.scene3D.fog = null;
        }
        let cameras;
        if (this.multipleViewsCheckBox.checked) {
            cameras = [this.fixedViewCamera, this.firstPersonViewCamera, this.thirdPersonViewCamera, this.topViewCamera];
        }
        else {
            cameras = [this.activeViewCamera];
        }
        for (const camera of cameras) {
            this.player.object.visible = (camera != this.firstPersonViewCamera);
            const viewport = camera.getViewport();
            this.renderer.setViewport(viewport.x, viewport.y, viewport.width, viewport.height);
            this.renderer.render(this.scene3D, camera.object);
            this.renderer.render(this.scene2D, this.camera2D);
            this.renderer.clearDepth();
        }

        // Render secondary viewport (mini-map)
        if (this.miniMapCheckBox.checked) {
            this.scene3D.fog = null;
            this.player.object.visible = true;
            const viewport = this.miniMapCamera.getViewport();
            this.renderer.setViewport(viewport.x, viewport.y, viewport.width, viewport.height);
            this.renderer.render(this.scene3D, this.miniMapCamera.object);
            this.renderer.render(this.scene2D, this.camera2D);
        }
    }
}
