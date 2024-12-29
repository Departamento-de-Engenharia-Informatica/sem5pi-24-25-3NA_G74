"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Allergy = void 0;
const AggregateRoot_1 = require("../core/domain/AggregateRoot");
const Result_1 = require("../core/logic/Result");
const Guard_1 = require("../core/logic/Guard");
class Allergy extends AggregateRoot_1.AggregateRoot {
    get id() {
        return this._id;
    }
    get code() {
        return this.props.code;
    }
    set code(value) {
        this.props.code = value;
    }
    get designation() {
        return this.props.designation;
    }
    set designation(value) {
        this.props.designation = value;
    }
    get description() {
        return this.props.description;
    }
    set description(value) {
        this.props.description = value;
    }
    constructor(props, id) {
        if (!Allergy.validateAllergyCode(props.code)) {
            throw new Error("Invalid medical condition code");
        }
        if (!Allergy.validateDesignationLenght(props.designation)) {
            throw new Error("Invalid designation length");
        }
        if (!Allergy.validateDescriptionLenght(props.description)) {
            throw new Error("Invalid description length");
        }
        super(props, id);
    }
    static validateAllergyCode(allergyCode) {
        const SNOMED_CT_PATTERN = /^\d{6,18}$/;
        const ICD_11_PATTERN = /^[A-Z][0-9A-Z]{1,2}\.[0-9A-Z]{1,4}$/;
        if (!allergyCode.match(SNOMED_CT_PATTERN) && !allergyCode.match(ICD_11_PATTERN)) {
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
    static create(allergyDTO, id) {
        const guardedProps = [
            { argument: allergyDTO.code, argumentName: 'code' },
            { argument: allergyDTO.designation, argumentName: 'designation' },
            { argument: allergyDTO.description, argumentName: 'description' }
        ];
        const guardResult = Guard_1.Guard.againstNullOrUndefinedBulk(guardedProps);
        if (!guardResult.succeeded) {
            return Result_1.Result.fail(guardResult.message);
        }
        else {
            const allergy = new Allergy({
                code: allergyDTO.code,
                description: allergyDTO.description,
                designation: allergyDTO.designation,
            }, id);
            return Result_1.Result.ok(allergy);
        }
    }
}
exports.Allergy = Allergy;
//# sourceMappingURL=Allergy.js.map