"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.MedicalConditionMap = void 0;
const Mapper_1 = require("../core/infra/Mapper");
const medicalCondition_1 = require("../domain/medicalCondition");
const UniqueEntityID_1 = require("../core/domain/UniqueEntityID");
class MedicalConditionMap extends Mapper_1.Mapper {
    static toDTO(medicalCondition) {
        return {
            id: medicalCondition.id.toString(),
            medicalConditionCode: medicalCondition.medicalConditionCode,
            designation: medicalCondition.designation,
            description: medicalCondition.description,
            commonSymptoms: medicalCondition.commonSymptoms
        };
    }
    static toDomain(raw) {
        const medicalConditionDTO = {
            id: raw._id,
            medicalConditionCode: raw.medicalConditionCode,
            designation: raw.designation,
            description: raw.description,
            commonSymptoms: raw.commonSymptoms
        };
        const medicalConditionOrError = medicalCondition_1.MedicalCondition.create(medicalConditionDTO, new UniqueEntityID_1.UniqueEntityID(raw.domainId));
        medicalConditionOrError.isFailure ? console.log(medicalConditionOrError.error) : '';
        return medicalConditionOrError.isSuccess ? medicalConditionOrError.getValue() : null;
    }
    static toPersistence(medicalCondition) {
        return {
            domainId: medicalCondition.id.toString(),
            medicalConditionCode: medicalCondition.medicalConditionCode,
            designation: medicalCondition.designation,
            description: medicalCondition.description,
            commonSymptoms: medicalCondition.commonSymptoms
        };
    }
}
exports.MedicalConditionMap = MedicalConditionMap;
//# sourceMappingURL=MedicalConditionMap.js.map