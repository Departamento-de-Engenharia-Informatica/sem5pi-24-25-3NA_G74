import * as THREE from "three";
import Ground from "./ground.js";
import Wall from "./wall.js";
import Door from "./door.js";


/*
 * parameters = {
 *  url: String,
 *  credits: String,
 *  scale: Vector3
 * }
 */

export default class Maze {
    constructor(parameters, doorParameters, initial_position) {

        this.onLoad = function (description) {

            // Store the maze's map and size
            this.map = description.map;
            this.size = description.size;

            console.log(description.map);
            // Store the building
            this.building = description.building;

            // Store the player's initial position and direction
            console.log('Pos__Initial = ', initial_position);
            if (initial_position == 0) {
                this.initialPosition = this.cellToCartesian(description.initialPosition);
            } else {
                this.initialPosition = this.cellToCartesian(initial_position);
            }

            console.log('PosInitial = ', this.initialPosition);

            this.initialDirection = description.initialDirection;

            // Store the maze's exit location
            this.exitLocation = this.cellToCartesian(description.exitLocation);

            if (typeof description.accessToBuilding != "undefined") {
                this.accessToBuilding = description.accessToBuilding;
            } else {
                this.accessToBuilding = [];
            }

            // Create a group of objects
            this.object = new THREE.Group();

            // Create the ground
            this.ground = new Ground({ textureUrl: description.groundTextureUrl, size: description.size });

            this.object.add(this.ground.object);

            // Create a wall
            this.wall = new Wall({ textureUrl: description.wallTextureUrl });

            // Build the maze
            let wallObject;

            this.doorObjectHorizontal = new Array();
            this.doorObjectVertical = new Array();
            this.doors = new Array();

            for (let i = 0; i <= description.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                for (let j = 0; j <= description.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
                    /*
           *  this.map[][] | North wall | West wall
           * --------------+------------+-----------
           *       0       |     No     |     No
           *       1       |     No     |    Yes
           *       2       |    Yes     |     No
           *       3       |    Yes     |    Yes
           *       4       |    door    |
           *       5       |            |    door
           */
                    if (description.map[j][i] == 2 || description.map[j][i] == 3) {
                        wallObject = this.wall.object.clone();
                        wallObject.position.set(i - description.size.width / 2.0 + 0.5, 0.5, j - description.size.height / 2.0);
                        this.object.add(wallObject);
                    }

                    if (description.map[j][i] == 1 || description.map[j][i] == 3) {
                        wallObject = this.wall.object.clone();
                        wallObject.rotateY(Math.PI / 2.0);
                        wallObject.position.set(i - description.size.width / 2.0, 0.5, j - description.size.height / 2.0 + 0.5);
                        this.object.add(wallObject);
                    }
                }
            }

            for (let i = 0; i <= description.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                for (let j = 0; j <= description.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
                    if (description.map[j][i] == 4) {
                        const position = [j - 0.56, i];
                        this.doorObjectHorizontal.push(this.cellToCartesian(position));
                        // Create the door
                        this.doorObject = new Door(doorParameters, [j, i]);
                        this.doors.push(this.doorObject);

                        // Add wall segment above door
                        let wallSegment = this.wall.object.clone();
                        wallSegment.scale.set(1, 0.05, 1);
                        wallSegment.position.set(i - description.size.width / 2.0 + 0.5, 2.1, j - description.size.height / 2.0);
                        this.object.add(wallSegment);
                    }
                }
            }

            for (let i = 0; i <= description.size.width; i++) { // In order to represent the eastmost walls, the map width is one column greater than the actual maze width
                for (let j = 0; j <= description.size.height; j++) { // In order to represent the southmost walls, the map height is one row greater than the actual maze height
                    if (description.map[j][i] == 5) {
                        const position = [j, i - 0.56];
                        this.doorObjectVertical.push(this.cellToCartesian(position));
                        // Create the door
                        this.doorObject = new Door(doorParameters, [j, i]);
                        this.doors.push(this.doorObject);

                        // Add wall segment above door
                        let wallSegment = this.wall.object.clone();
                        wallSegment.scale.set(1, 0.05, 1); // Adjust the 0.3 value to match the height you want
                        wallSegment.rotateY(Math.PI / 2.0);
                        wallSegment.position.set(i - description.size.width / 2.0, 2.1, j - description.size.height / 2.0 + 0.5); // Adjust 1.3 to position it right above the door
                        this.object.add(wallSegment);
                    }
                }
            }


            this.object.scale.set(this.scale.x, this.scale.y, this.scale.z);
            this.loaded = true;
        }

        this.onProgress = function (url, xhr) {
            console.log("Resource '" + url + "' " + (100.0 * xhr.loaded / xhr.total).toFixed(0) + "% loaded.");
        }

        this.onError = function (url, error) {
            console.error("Error loading resource " + url + " (" + error + ").");
        }

        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }
        this.loaded = false;

        // The cache must be enabled; additional information available at https://threejs.org/docs/api/en/loaders/FileLoader.html
        THREE.Cache.enabled = true;

        // Create a resource file loader
        const loader = new THREE.FileLoader();

        // Set the response type: the resource file will be parsed with JSON.parse()
        loader.setResponseType("json");

        // Load a maze description resource file
        loader.load(
            //Resource URL
            this.url,

            // onLoad callback
            description => this.onLoad(description),

            // onProgress callback
            xhr => this.onProgress(this.url, xhr),

            // onError callback
            error => this.onError(this.url, error)
        );
    }

    // Convert cell [row, column] coordinates to cartesian (x, y, z) coordinates
    cellToCartesianWithHeight(position) {
        return new THREE.Vector3((position[1] - this.size.width / 2.0 + 0.5) * this.scale.x, (position[2] - this.size.width / 2.0 + 0.5) * this.scale.y, (position[0] - this.size.height / 2.0 + 0.5) * this.scale.z)
    }

    cellToCartesian(position) {
        return new THREE.Vector3((position[1] - this.size.width / 2.0 + 0.5) * this.scale.x, 0.0, (position[0] - this.size.height / 2.0 + 0.5) * this.scale.z)
    }

    // Convert cartesian (x, y, z) coordinates to cell [row, column] coordinates
    cartesianToCell(position) {
        return [Math.floor(position.z / this.scale.z + this.size.height / 2.0), Math.floor(position.x / this.scale.x + this.size.width / 2.0)];
    }

    distanceToWestWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return position.x - this.cellToCartesian(indices).x + this.scale.x / 2.0;
        }
        return Infinity;
    }

    distanceToEastWall(position) {
        const indices = this.cartesianToCell(position);
        indices[1]++;
        if (this.map[indices[0]][indices[1]] == 1 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).x - this.scale.x / 2.0 - position.x;
        }
        return Infinity;
    }

    distanceToNorthWall(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return position.z - this.cellToCartesian(indices).z + this.scale.z / 2.0;
        }
        return Infinity;
    }

    distanceToSouthWall(position) {
        const indices = this.cartesianToCell(position);
        indices[0]++;
        if (this.map[indices[0]][indices[1]] == 2 || this.map[indices[0]][indices[1]] == 3) {
            return this.cellToCartesian(indices).z - this.scale.z / 2.0 - position.z;
        }
        return Infinity;
    }

    distanceToEastDoor(position) {
        const indices = this.cartesianToCell(position);
        indices[1]++;
        if (this.map[indices[0]][indices[1]] == 5) {
            return this.cellToCartesian(indices).x - this.scale.x / 2.0 - position.x;
        }
        return Infinity;
    }

    distanceToWestDoor(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 5) {
            return position.x - this.cellToCartesian(indices).x + this.scale.x / 2.0;
        }
        return Infinity;
    }

    distanceToNorthDoor(position) {
        const indices = this.cartesianToCell(position);
        if (this.map[indices[0]][indices[1]] == 4) {
            return position.z - this.cellToCartesian(indices).z + this.scale.z / 2.0;
        }
        return Infinity;
    }

    distanceToSouthDoor(position) {
        const indices = this.cartesianToCell(position);
        indices[0]++;
        if (this.map[indices[0]][indices[1]] == 4) {
            return this.cellToCartesian(indices).z - this.scale.z / 2.0 - position.z;
        }
        return Infinity;
    }


    foundExit(position) {
        return Math.abs(position.x - this.exitLocation.x) < 0.5 * this.scale.x && Math.abs(position.z - this.exitLocation.z) < 0.5 * this.scale.z
    };



}
