import { expect } from "chai";
import { MedicalCondition } from "../../../src/domain/medicalCondition";
import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";
import { IMedicalConditionDTO } from "../../../src/dto/IMedicalConditionDTO";

describe("MedicalCondition", () => {
    const validDTO: IMedicalConditionDTO = {
        medicalConditionCode: "A12.34", // Valid ICD-11 format
        description: "This is a valid description of a medical condition.",
        designation: "Condition Name",
        commonSymptoms: "Fever, cough, fatigue",
    };

    const invalidCodeDTO: IMedicalConditionDTO = {
        ...validDTO,
        medicalConditionCode: "INVALID_CODE", // Invalid code
    };

    const longDesignationDTO: IMedicalConditionDTO = {
        ...validDTO,
        designation: "x".repeat(101), // Exceeds 100 characters
    };

    const longDescriptionDTO: IMedicalConditionDTO = {
        ...validDTO,
        description: "x".repeat(2049), // Exceeds 2048 characters
    };

    it("should successfully create a valid MedicalCondition", () => {
        const result = MedicalCondition.create(validDTO);

        expect(result.isSuccess).to.be.true;
        expect(result.getValue()).to.be.instanceOf(MedicalCondition);

        const medicalCondition = result.getValue();
        expect(medicalCondition!.medicalConditionCode).to.equal(validDTO.medicalConditionCode);
        expect(medicalCondition!.description).to.equal(validDTO.description);
        expect(medicalCondition!.designation).to.equal(validDTO.designation);
        expect(medicalCondition!.commonSymptoms).to.equal(validDTO.commonSymptoms);
    });

    it("should fail to create a MedicalCondition with an invalid medical condition code", () => {
        expect(() => {
            MedicalCondition.create(invalidCodeDTO);
        }).to.throw("Invalid medical condition code");
    });
    
    it("should fail to create a MedicalCondition with a long designation", () => {
        expect(() => {
            MedicalCondition.create(longDesignationDTO);
        }).to.throw("Invalid designation length");
    });
    
    it("should fail to create a MedicalCondition with a long description", () => {
        expect(() => {
            MedicalCondition.create(longDescriptionDTO);
        }).to.throw("Invalid description length");
    });
    

    it("should throw an error for invalid medical condition code during instantiation", () => {
        expect(() => {
            MedicalCondition.create(
                { ...validDTO, medicalConditionCode: "INVALID_CODE" },
                new UniqueEntityID()
            );
        }).to.throw("Invalid medical condition code");
    });

    it("should set and get properties correctly", () => {
        const result = MedicalCondition.create(validDTO);
        expect(result.isSuccess).to.be.true;

        const medicalCondition = result.getValue();

        medicalCondition!.designation = "New Designation";
        expect(medicalCondition!.designation).to.equal("New Designation");

        medicalCondition!.description = "Updated description";
        expect(medicalCondition!.description).to.equal("Updated description");
    });
});
