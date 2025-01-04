import { expect } from "chai";
import sinon from "sinon";

import { Result } from "../../../src/core/logic/Result";
import IAllergyRepo from "../../../src/services/IRepos/IAllergyRepo";
import {IAllergyDTO} from "../../../src/dto/IAllergyDTO";
import {Allergy} from "../../../src/domain/Allergy";
import {ALL} from "node:dns";
import {AllergyMap} from "../../../src/mappers/AllergyMap";
import AllergyService from "../../../src/services/allergyService";

class MockAllergyRepo implements IAllergyRepo {
    exists = sinon.stub();
    save = sinon.stub();
    update = sinon.stub();
    findByCode = sinon.stub();
    findByDesignation = sinon.stub();
    findAll = sinon.stub();
    findById = sinon.stub();
    findByDescription = sinon.stub();
}

describe("AllergyService", () => {
    let allergyRepo: MockAllergyRepo;
    let allergyService: AllergyService;

    const mockAllergyDTO: IAllergyDTO = {
        code: "A12.34",
        description: "Valid description",
        designation: "Valid designation",
    };

    beforeEach(() => {
        allergyRepo = new MockAllergyRepo();
        allergyService = new AllergyService(allergyRepo as any);
    });

    afterEach(() => {
        sinon.restore();
    });

    describe("CreateAllergy", () => {
        it("should successfully create and save a allergy", async () => {
            const allergy = Allergy.create(mockAllergyDTO).getValue();
            sinon.stub(Allergy, "create").returns(Result.ok(allergy));
            allergyRepo.save.resolves();

            const result = await allergyService.CreateAllergy(mockAllergyDTO);

            expect(result.isSuccess).to.be.true;
            expect(allergyRepo.save.calledOnce).to.be.true;
        });

        it("should fail to create a allergy when validation fails", async () => {
            sinon.stub(Allergy, "create").returns(Result.fail("Validation failed"));

            const result = await allergyService.CreateAllergy(mockAllergyDTO);

            expect(result.isSuccess).to.be.false;
            expect(result.error).to.equal("Validation failed");
            expect(allergyRepo.save.called).to.be.false;
        });
    });

    describe("UpdateAllergy", () => {
        it("should successfully update a allergy", async () => {
            const allergy = Allergy.create(mockAllergyDTO).getValue();
            allergyRepo.findByCode.resolves(allergy);
            allergyRepo.save.resolves();

            const dto = { ...mockAllergyDTO, designation: "Updated Designation" };
            const result = await allergyService.UpdateAllergy(dto);

            expect(result.isSuccess).to.be.true;
            expect(allergyRepo.findByCode.calledOnce).to.be.true;
            expect(allergyRepo.save.calledOnce).to.be.true;
            expect(allergy.designation).to.equal("Updated Designation");
        });

        it("should fail to update a allergy if it is not found", async () => {
            allergyRepo.findByCode.resolves(null);

            const result = await allergyService.UpdateAllergy(mockAllergyDTO);

            expect(result.isSuccess).to.be.false;
            expect(result.error).to.equal("Allergy not found");
            expect(allergyRepo.save.called).to.be.false;
        });
    });

    describe("SearchAllergy", () => {
        it("should find a allergy by code", async () => {
            const allergy = sinon.createStubInstance(Allergy);
            allergyRepo.findByCode.resolves(allergy);
            sinon.stub(AllergyMap, "toDTO").returns(mockAllergyDTO);

            const result = await allergyService.SearchAllergy("A12.34");

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.have.length(1);
            expect(result.getValue()[0].code).to.equal("A12.34");
        });

        it("should find allergy by designation", async () => {
            const allergies = [sinon.createStubInstance(Allergy)];
            allergyRepo.findByDesignation.resolves(allergies);
            sinon.stub(AllergyMap, "toDTO").returns(mockAllergyDTO);

            const result = await allergyService.SearchAllergy(undefined, "Valid designation");

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.be.an("array").that.has.length(1);
            expect(result.getValue()[0].designation).to.equal("Valid designation");
        });

        it("should find all allergies when no parameters are provided", async () => {
            const allergies = [sinon.createStubInstance(Allergy)];
            allergyRepo.findAll.resolves(allergies);
            sinon.stub(AllergyMap, "toDTO").returns(mockAllergyDTO);

            const result = await allergyService.SearchAllergy();

            expect(result.isSuccess).to.be.true;
            expect(result.getValue()).to.be.an("array").that.is.not.empty;
        });
        
    });
});
