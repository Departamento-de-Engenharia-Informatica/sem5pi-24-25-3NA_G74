"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MedicalConditionId = void 0;
const Entity_1 = require("../core/domain/Entity");
class MedicalConditionId extends Entity_1.Entity {
    get id() {
        return this._id;
    }
    constructor(id) {
        super(null, id);
    }
}
exports.MedicalConditionId = MedicalConditionId;
//# sourceMappingURL=medicalConditionId.js.map