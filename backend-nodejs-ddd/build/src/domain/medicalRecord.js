"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MedicalRecord = void 0;
const AggregateRoot_1 = require("../core/domain/AggregateRoot");
class MedicalRecord extends AggregateRoot_1.AggregateRoot {
    get getId() {
        return this._id;
    }
    get getAllergies() {
        return this.props.allergies;
    }
    get getMedicalCondition() {
        return this.props.medicalCondition;
    }
    get getDesignation() {
        return this.props.designation;
    }
    set setAllergies(value) {
        this.props.allergies = value;
    }
    set setMedicalCondition(value) {
        this.props.medicalCondition = value;
    }
    set setDesignation(value) {
        this.props.designation = value;
    }
    constructor(props, id) {
        super(props, id);
    }
}
exports.MedicalRecord = MedicalRecord;
//# sourceMappingURL=medicalRecord.js.map