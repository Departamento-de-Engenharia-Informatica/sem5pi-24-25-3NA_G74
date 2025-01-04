"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.AllergyMap = void 0;
const Mapper_1 = require("../core/infra/Mapper");
const Allergy_1 = require("../domain/Allergy");
const UniqueEntityID_1 = require("../core/domain/UniqueEntityID");
class AllergyMap extends Mapper_1.Mapper {
    static toDTO(allergy) {
        return {
            id: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description,
        };
    }
    static toDomain(raw) {
        const allergyDTO = {
            id: raw._id,
            code: raw.code,
            designation: raw.designation,
            description: raw.description
        };
        const allergyOrError = Allergy_1.Allergy.create(allergyDTO, new UniqueEntityID_1.UniqueEntityID(raw.domainId));
        allergyOrError.isFailure ? console.log(allergyOrError.error) : '';
        return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
    }
    static toPersistence(allergy) {
        return {
            domainId: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description
        };
    }
}
exports.AllergyMap = AllergyMap;
//# sourceMappingURL=AllergyMap.js.map