import { expect } from "chai";
import mongoose from "mongoose";
import { MongoMemoryServer } from "mongodb-memory-server";
import sinon from "sinon";

import MedicalConditionService from "../../src/services/MedicalConditionService";
import MedicalConditionRepo from "../../src/repos/MedicalConditionRepo";
import medicalConditionSchema from "../../src/persistence/schemas/medicalConditionSchema";
// e.g. your actual Mongoose schema
import { IMedicalConditionDTO } from "../../src/dto/IMedicalConditionDTO";
import { Result } from "../../src/core/logic/Result";

// Or import from your config/typedi, adapt as needed
import { Model } from "mongoose";
import { IMedicalConditionPersistence } from "../../src/dataschema/IMedicalConditionPersistence";

// This is a real test using an in-memory DB
describe("Integration - MedicalConditionService + MedicalConditionRepo + in-memory Mongo", () => {
    let mongoServer: MongoMemoryServer;
    let repo: MedicalConditionRepo;
    let service: MedicalConditionService;
    let medicalConditionModel: Model<any>;

    const MedicalConditionSchema = new mongoose.Schema(
        {
            domainId: { type: String, unique: true },
            medicalConditionCode: { type: String, unique: true },
            designation: { type: String },
            description: { type: String },
            commonSymptoms: { type: String }
        },
        {
            timestamps: true
        }
    );

    const validDTO: IMedicalConditionDTO = {
        medicalConditionCode: "A12.34",
        description: "Test Desc",
        designation: "Test Designation",
        commonSymptoms: "Cough, Fever"
    };

    before(async () => {
        mongoServer = await MongoMemoryServer.create();
        const uri = mongoServer.getUri();

        await mongoose.connect(uri, { dbName: "testdb" });

        if (mongoose.models["MedicalCondition"]) {
            delete mongoose.models["MedicalCondition"];
        }


        medicalConditionModel = mongoose.model<IMedicalConditionPersistence & mongoose.Document>("MedicalCondition", MedicalConditionSchema);
        repo = new MedicalConditionRepo(medicalConditionModel, console);
        service = new MedicalConditionService(repo);
    });

    after(async () => {
        await mongoose.disconnect();
        await mongoServer.stop();
    });

    afterEach(() => {
        sinon.restore();
    });

    describe("createMedicalCondition", () => {
        it("should create a new medical condition in the in-memory DB", async () => {
            // Act
            const result = await service.createMedicalCondition(validDTO);

            // Assert
            expect(result.isSuccess).to.be.true;
            expect(result.getValue().medicalConditionCode).to.equal("A12.34");

            // Now check the DB directly:
            const found = await medicalConditionModel.findOne({ medicalConditionCode: "A12.34" }).exec();
            expect(found).to.exist;
            expect(found.description).to.equal("Test Desc");
        });
    });

    describe("UpdateMedicalCondition", () => {
        it("should update a medical condition in the DB", async () => {
            // First create
            await service.createMedicalCondition(validDTO);

            // Now update the designation
            const updatedDTO = { ...validDTO, designation: "New Designation" };
            const updateResult = await service.UpdateMedicalCondition(updatedDTO);

            expect(updateResult.isSuccess).to.be.true;
            const found = await medicalConditionModel
                .findOne({ medicalConditionCode: "A12.34" })
                .exec();

            expect(found.designation).to.equal("New Designation");
        });
    });

    describe("SearchMedicalCondition", () => {
        it("should return array of items by code if found", async () => {
            // Insert something
            await service.createMedicalCondition(validDTO);

            // Search by code
            const searchResult = await service.SearchMedicalCondition("A12.34");
            expect(searchResult.isSuccess).to.be.true;
            expect(searchResult.getValue()).to.have.length(1);
        });
    });
});
