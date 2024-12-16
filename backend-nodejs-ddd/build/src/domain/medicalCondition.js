"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MedicalCondition = void 0;
const AggregateRoot_1 = require("../core/domain/AggregateRoot");
const Guard_1 = require("../core/logic/Guard");
const Result_1 = require("../core/logic/Result");
class MedicalCondition extends AggregateRoot_1.AggregateRoot {
    get Id() {
        return this._id;
    }
    get description() {
        return this.props.description;
    }
    set description(value) {
        this.props.description = value;
    }
    get medicalConditionCode() {
        return this.props.medicalConditionCode;
    }
    set medicalConditionCode(value) {
        this.props.medicalConditionCode = value;
    }
    get designation() {
        return this.props.designation;
    }
    set designation(value) {
        this.props.designation = value;
    }
    get commonSymptoms() {
        return this.props.commonSymptoms;
    }
    set commonSymptoms(value) {
        this.props.commonSymptoms = value;
    }
    constructor(props, id) {
        if (!MedicalCondition.validateMedicalConditionCode(props.medicalConditionCode)) {
            throw new Error("Invalid medical condition code");
        }
        if (!MedicalCondition.validateDesignationLenght(props.designation)) {
            throw new Error("Invalid designation length");
        }
        if (!MedicalCondition.validateDescriptionLenght(props.description)) {
            throw new Error("Invalid description length");
        }
        super(props, id);
    }
    static validateMedicalConditionCode(medicalConditionCode) {
        const SNOMED_CT_PATTERN = /^\d{6,18}$/;
        const ICD_11_PATTERN = /^[A-Z][0-9A-Z]{1,2}\.[0-9A-Z]{1,4}$/;
        if (!medicalConditionCode.match(SNOMED_CT_PATTERN) && !medicalConditionCode.match(ICD_11_PATTERN)) {
            return false;
        }
        return true;
    }
    static validateDesignationLenght(designation) {
        if (designation.length > 100) {
            return false;
        }
        return true;
    }
    static validateDescriptionLenght(description) {
        if (description.length > 2048) {
            return false;
        }
        return true;
    }
    static create(medicalConditionDTO, id) {
        const guardedProps = [
            { argument: medicalConditionDTO.medicalConditionCode, argumentName: 'medicalConditionCode' },
            { argument: medicalConditionDTO.description, argumentName: 'description' },
            { argument: medicalConditionDTO.designation, argumentName: 'designation' },
            { argument: medicalConditionDTO.commonSymptoms, argumentName: 'commonSymptoms' }
        ];
        const guardResult = Guard_1.Guard.againstNullOrUndefinedBulk(guardedProps);
        if (!guardResult.succeeded) {
            return Result_1.Result.fail(guardResult.message);
        }
        else {
            const medicalCondition = new MedicalCondition({
                medicalConditionCode: medicalConditionDTO.medicalConditionCode,
                description: medicalConditionDTO.description,
                designation: medicalConditionDTO.designation,
                commonSymptoms: medicalConditionDTO.commonSymptoms
            }, id);
            return Result_1.Result.ok(medicalCondition);
        }
    }
}
exports.MedicalCondition = MedicalCondition;
//# sourceMappingURL=medicalCondition.js.map