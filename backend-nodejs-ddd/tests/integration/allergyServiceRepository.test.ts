import { expect } from "chai";
import mongoose from "mongoose";
import { MongoMemoryServer } from "mongodb-memory-server";
import sinon from "sinon";
import { Model } from "mongoose";
import AllergyRepo from "../../src/repos/allergyRepo";
import AllergyService from "../../src/services/allergyService";
import {IAllergyDTO} from "../../src/dto/IAllergyDTO";
import {IAllergyPersistence} from "../../src/dataschema/IAllergyPersistence";

// This is a real test using an in-memory DB
describe("Integration - AllergyService + AllergyRepo + in-memory Mongo", () => {
    let mongoServer: MongoMemoryServer;
    let allergyRepo: AllergyRepo;
    let allergyService: AllergyService;
    let allergyModel: Model<any>;

    const AllergySchema = new mongoose.Schema(
        {
            domainId: { type: String, unique: true },
            code: { type: String, unique: true },
            designation: { type: String },
            description: { type: String }
        },
        {
            timestamps: true
        }
    );

    const validDTO: IAllergyDTO = {
        code: "A12.34",
        description: "Test Desc",
        designation: "Test Designation"
    };

    before(async () => {
        mongoServer = await MongoMemoryServer.create();
        const uri = mongoServer.getUri();

        await mongoose.connect(uri, { dbName: "testdb" });

        if (mongoose.models["Allergy"]) {
            delete mongoose.models["Allergy"];
        }


        allergyModel = mongoose.model<IAllergyPersistence & mongoose.Document>("Allergy", AllergySchema);
        allergyRepo = new AllergyRepo(allergyModel, console);
        allergyService = new AllergyService(allergyRepo);
    });

    after(async () => {
        await mongoose.disconnect();
        await mongoServer.stop();
    });

    afterEach(() => {
        sinon.restore();
    });

    describe("CreateAllergy", () => {
        it("should create a new allergy in the in-memory DB", async () => {
            // Act
            const result = await allergyService.CreateAllergy(validDTO);

            // Assert
            expect(result.isSuccess).to.be.true;
            expect(result.getValue().code).to.equal("A12.34");

            // Now check the DB directly:
            const found = await allergyModel.findOne({ code: "A12.34" }).exec();
            expect(found).to.exist;
            expect(found.description).to.equal("Test Desc");
        });
    });

    describe("UpdateAllergy", () => {
        it("should update a allergy in the DB", async () => {
            // First create
            await allergyService.CreateAllergy(validDTO);

            // Now update the designation
            const updatedDTO = { ...validDTO, designation: "New Designation" };
            const updateResult = await allergyService.UpdateAllergy(updatedDTO);

            expect(updateResult.isSuccess).to.be.true;
            const found = await allergyModel
                .findOne({ code: "A12.34" })
                .exec();

            expect(found.designation).to.equal("New Designation");
        });
    });

    describe("SearchAllergy", () => {
        it("should return array of items by code if found", async () => {
            // Insert something
            await allergyService.CreateAllergy(validDTO);

            // Search by code
            const searchResult = await allergyService.SearchAllergy("A12.34");
            expect(searchResult.isSuccess).to.be.true;
            expect(searchResult.getValue()).to.have.length(1);
        });
    });
});
