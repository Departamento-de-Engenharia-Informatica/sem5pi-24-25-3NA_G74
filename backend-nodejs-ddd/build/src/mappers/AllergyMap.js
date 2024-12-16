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
            designation: allergy.designation
        };
    }
    static async toDomain(raw) {
        const allergyOrError = Allergy_1.Allergy.create({
            code: raw.code,
            designation: raw.designation,
            description: raw.description,
        }, new UniqueEntityID_1.UniqueEntityID(raw.domainId));
        allergyOrError.isFailure ? console.log(allergyOrError.error) : '';
        return allergyOrError.isSuccess ? allergyOrError.getValue() : null;
    }
    static toPersistence(allergy) {
        const a = {
            domainId: allergy.id.toString(),
            code: allergy.code,
            designation: allergy.designation,
            description: allergy.description
        };
        return a;
    }
}
exports.AllergyMap = AllergyMap;
//# sourceMappingURL=AllergyMap.js.map