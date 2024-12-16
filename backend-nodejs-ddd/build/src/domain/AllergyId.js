"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.AllergyId = void 0;
const Entity_1 = require("../core/domain/Entity");
class AllergyId extends Entity_1.Entity {
    get id() {
        return this._id;
    }
    constructor(id) {
        super(null, id);
    }
}
exports.AllergyId = AllergyId;
//# sourceMappingURL=AllergyId.js.map