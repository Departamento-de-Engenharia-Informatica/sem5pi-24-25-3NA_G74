import { expect } from "chai";
import sinon from "sinon";
import MedicalConditionService from "../../../src/services/MedicalConditionService";
import IMedicalConditionRepo from "../../../src/services/IRepos/IMedicalConditionRepo";
import { MedicalCondition } from "../../../src/domain/medicalCondition";
import { MedicalConditionMap } from "../../../src/mappers/MedicalConditionMap";
import { IMedicalConditionDTO } from "../../../src/dto/IMedicalConditionDTO";
import { Result } from "../../../src/core/logic/Result";

class MockMedicalConditionRepo implements IMedicalConditionRepo {
    exists = sinon.stub();
    save = sinon.stub();
    update = sinon.stub();
    findByDescription = sinon.stub();
    findByDesignation = sinon.stub();
    findAll = sinon.stub();
    findById = sinon.stub();
    findByMedicalConditionCode = sinon.stub();
}

describe("MedicalConditionService", () => {
    let medicalConditionRepo: MockMedicalConditionRepo;
    let medicalConditionService: MedicalConditionService;

    const mockMedicalConditionDTO: IMedicalConditionDTO = {
        medicalConditionCode: "A12.34",
        description: "Valid description",
        designation: "Valid designation",
        commonSymptoms: "Fever, cough",
    };

    beforeEach(() => {
        medicalConditionRepo = new MockMedicalConditionRepo();
        medicalConditionService = new MedicalConditionService(medicalConditionRepo as any);
    });

    afterEach(() => {
        sinon.restore();
    });

    describe("createMedicalCondition", () => {
        it("should successfully create and save a medical condition", async () => {
            const medicalCondition = MedicalCondition.create(mockMedicalConditionDTO).getValue();
            sinon.stub(MedicalCondition, "create").returns(Result.ok(medicalCondition));
            medicalConditionRepo.save.resolves();

            const result = await medicalConditionService.createMedicalCondition(mockMedicalConditionDTO);

            expect(result.isSuccess).to.be.true;
            expect(medicalConditionRepo.save.calledOnce).to.be.true;
        });

        it("should fail to create a medical condition when validation fails", async () => {
            sinon.stub(MedicalCondition, "create").returns(Result.fail("Validation failed"));

            const result = await medicalConditionService.createMedicalCondition(mockMedicalConditionDTO);

            expect(result.isSuccess).to.be.false;
            expect(result.error).to.equal("Validation failed");
            expect(medicalConditionRepo.save.called).to.be.false;
        });
    });

    describe("UpdateMedicalCondition", () => {
        it("should successfully update a medical condition", async () => {
            const medicalCondition = MedicalCondition.create(mockMedicalConditionDTO).getValue();
            medicalConditionRepo.findByMedicalConditionCode.resolves(medicalCondition);
            medicalConditionRepo.save.resolves();

            const dto = { ...mockMedicalConditionDTO, designation: "Updated Designation" };
            const result = await medicalConditionService.UpdateMedicalCondition(dto);

            expect(result.isSuccess).to.be.true;
            expect(medicalConditionRepo.findByMedicalConditionCode.calledOnce).to.be.true;
            expect(medicalConditionRepo.save.calledOnce).to.be.true;
            expect(medicalCondition.designation).to.equal("Updated Designation");
        });

        it("should fail to update a medical condition if it is not found", async () => {
            medicalConditionRepo.findByMedicalConditionCode.resolves(null);

            const result = await medicalConditionService.UpdateMedicalCondition(mockMedicalConditionDTO);

            expect(result.isSuccess).to.be.false;
            expect(result.error).to.equal("Medical Condition not found");
            expect(medicalConditionRepo.save.called).to.be.false;
        });
    });

    describe("SearchMedicalCondition", () => {
        it("should find a medical condition by code", async () => {
            const medicalCondition = sinon.createStubInstance(MedicalCondition);
            medicalConditionRepo.findByMedicalConditionCode.resolves(medicalCondition);
            sinon.stub(MedicalConditionMap, "toDTO").returns(mockMedicalConditionDTO);

            const result = await medicalConditionService.SearchMedicalCondition("A12.34");

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.have.length(1);
            expect(result.getValue()[0].medicalConditionCode).to.equal("A12.34");
        });

        it("should find medical conditions by designation", async () => {
            const medicalConditions = [sinon.createStubInstance(MedicalCondition)];
            medicalConditionRepo.findByDesignation.resolves(medicalConditions);
            sinon.stub(MedicalConditionMap, "toDTO").returns(mockMedicalConditionDTO);

            const result = await medicalConditionService.SearchMedicalCondition(undefined, "Valid designation");

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.be.an("array").that.has.length(1);
            expect(result.getValue()[0].designation).to.equal("Valid designation");
        });

        it("should find all medical conditions when no parameters are provided", async () => {
            const medicalConditions = [sinon.createStubInstance(MedicalCondition)];
            medicalConditionRepo.findAll.resolves(medicalConditions);
            sinon.stub(MedicalConditionMap, "toDTO").returns(mockMedicalConditionDTO);

            const result = await medicalConditionService.SearchMedicalCondition();

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.be.an("array").that.is.not.empty;
        });

        it("should return an empty array when no medical conditions are found", async () => {
            medicalConditionRepo.findByMedicalConditionCode.resolves(null);

            const result = await medicalConditionService.SearchMedicalCondition("NON_EXISTENT_CODE");

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.be.an("array").that.is.empty;
        });
    });
});
