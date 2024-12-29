"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MedicalRecord = void 0;
const AggregateRoot_1 = require("../core/domain/AggregateRoot");
const Result_1 = require("../core/logic/Result");
const medicalRecordId_1 = require("./medicalRecordId");
class MedicalRecord extends AggregateRoot_1.AggregateRoot {
    get id() {
        return this._id;
    }
    //Isto precisa ser revisto Pina
    get medicalRecordId() {
        return new medicalRecordId_1.MedicalRecordId( /*this.medicalRecordId.toValue()*/);
    }
    get allergies() {
        return this.props.allergies;
    }
    get medicalConditions() {
        return this.props.medicalConditions;
    }
    get freeText() {
        return this.props.freeText;
    }
    set allergies(value) {
        this.props.allergies = value;
    }
    set medicalConditions(value) {
        this.props.medicalConditions = value;
    }
    set freeText(value) {
        this.props.freeText = value;
    }
    constructor(props, id) {
        super(props, id);
    }
    static create(medicalRecordDTO, id) {
        // Basic validation
        if (!medicalRecordDTO) {
            return Result_1.Result.fail('Medical Record DTO is required');
        }
        const medicalRecord = new MedicalRecord({
            allergies: medicalRecordDTO.allergies || [],
            medicalConditions: medicalRecordDTO.medicalConditions || [],
            freeText: medicalRecordDTO.freeText || '',
        }, id);
        return Result_1.Result.ok(medicalRecord);
    }
}
exports.MedicalRecord = MedicalRecord;
//# sourceMappingURL=medicalRecord.js.map