import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";
import mongoose, { Model } from "mongoose";
import { MongoMemoryServer } from "mongodb-memory-server";

// Your real code:
import MedicalConditionController from "../../src/controllers/MedicalConditionController";
import MedicalConditionService from "../../src/services/MedicalConditionService";
import MedicalConditionRepo from "../../src/repos/MedicalConditionRepo";
import { IMedicalConditionPersistence } from "../../src/dataschema/IMedicalConditionPersistence";
// ^ adapt this import to wherever your Mongoose schema is
import { IMedicalConditionDTO } from "../../src/dto/IMedicalConditionDTO";
import { Result } from "../../src/core/logic/Result";

// You might also need the config for the @Inject decorators, or you can pass them manually.

describe("Full Chain Test - (Controller + Service + Repository + In-memory Mongo) WITHOUT starting Express", () => {
    let mongoServer: MongoMemoryServer;
    let medicalConditionModel: Model<any>;
    let medicalConditionRepo: MedicalConditionRepo;
    let medicalConditionService: MedicalConditionService;
    let medicalConditionController: MedicalConditionController;

    let req: Partial<Request>;
    let res: SinonStubbedInstance<Response>;
    let next: sinon.SinonStub;

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

    // Sample DTO we'll use
    const sampleDTO: IMedicalConditionDTO = {
        medicalConditionCode: "123456",
        description: "Some description",
        designation: "Some designation",
        commonSymptoms: "Fever, chills"
    };

    before(async () => {
        // 1) Spin up the in-memory server
        mongoServer = await MongoMemoryServer.create();
        const uri = mongoServer.getUri();

        // 2) Connect mongoose to our ephemeral instance
        await mongoose.connect(uri, { dbName: "testdb" });

        // 3) Create your model from the actual schema
        medicalConditionModel = mongoose.model<IMedicalConditionPersistence & mongoose.Document>("MedicalCondition", MedicalConditionSchema);

        // 4) Instantiate real Repo, Service, Controller
        medicalConditionRepo = new MedicalConditionRepo(medicalConditionModel, console);
        medicalConditionService = new MedicalConditionService(medicalConditionRepo);
        medicalConditionController = new MedicalConditionController(medicalConditionService);
    });

    after(async () => {
        // Disconnect and stop in-memory server
        await mongoose.disconnect();
        await mongoServer.stop();
    });

    beforeEach(() => {
        // Create fresh stubs for req/res/next
        req = { body: {}, params: {}, query: {} } as Partial<Request>;
        res = {
            status: sinon.stub().returnsThis(),
            json: sinon.stub().returnsThis(),
            send: sinon.stub().returnsThis()
        } as unknown as SinonStubbedInstance<Response>;
        next = sinon.stub();

        // Stub out BaseController methods to avoid real HTTP calls
        sinon.stub(medicalConditionController, "fail").callsFake((error: any) => {
            res.status(500).send(error instanceof Error ? error.message : String(error));
            return res;
        });
        sinon.stub(medicalConditionController, "notFound").callsFake((message?: unknown) => {
            res.status(404).send(typeof message === "string" ? message : "Not found");
            return res as unknown as Response;
        });
    });

    afterEach(async () => {
        // Clear the collection after each test if you want them truly isolated
        await medicalConditionModel.deleteMany({});
        sinon.restore();
    });

    it("should create a new medical condition and persist it in the in-memory DB", async () => {
        // Arrange
        req.body = sampleDTO;

        // Act
        await medicalConditionController.createMedicalCondition(
            req as Request,
            res as Response,
            next as NextFunction
        );

        // Assert - Check that the response was 201, contained the correct JSON
        expect(res.status.calledWith(201)).to.be.true;
        expect(res.json.called).to.be.true;
        const returnedData = res.json.getCall(0).args[0]; // The argument to res.json()
        expect(returnedData).to.include({
            medicalConditionCode: "123456",
            description: "Some description",
            designation: "Some designation"
        });

        // Also check that the data was really persisted in the DB
        const doc = await medicalConditionModel.findOne({ medicalConditionCode: "123456" });
        expect(doc).to.exist;
        expect(doc.description).to.equal("Some description");
    });

    it("should update a medical condition in the DB via the controller", async () => {
        // First, create a record manually
        await medicalConditionModel.create({
            medicalConditionCode: sampleDTO.medicalConditionCode,
            description: sampleDTO.description,
            designation: sampleDTO.designation,
            commonSymptoms: sampleDTO.commonSymptoms,
            domainId: "some-random-id"
            // ... any other fields needed by your schema
        });

        // Now we want to update the 'designation' via controller
        req.params = { medicalConditionCode: "123456" };
        req.body = { designation: "Updated Designation" };

        // Call controller
        await medicalConditionController.updateMedicalCondition(
            req as Request,
            res as Response,
            next as NextFunction
        );

        // Expect 200 status and updated object in the response
        expect(res.status.calledWith(200)).to.be.true;
        const updatedData = res.json.getCall(0).args[0];
        expect(updatedData).to.include({
            medicalConditionCode: "123456",
            designation: "Updated Designation"
        });

        // Verify in DB
        const doc = await medicalConditionModel.findOne({ medicalConditionCode: "123456" });
        expect(doc.designation).to.equal("Updated Designation");
    });

    

    it("should search and find a record by code", async () => {
        // Insert record directly into DB
        await medicalConditionModel.create({
            medicalConditionCode: "123488",
            description: "Search desc",
            designation: "Search design",
            commonSymptoms: "Search symptoms",
            domainId: "some-other-id"
        });

        // Now call controller: search by code
        req.query = { medicalConditionCode: "123488" };
        await medicalConditionController.searchMedicalCondition(
            req as Request,
            res as Response,
            next as NextFunction
        );

        // Should get 200 + array with 1 item
        expect(res.status.calledWith(200)).to.be.true;
        const foundItems = res.json.getCall(0).args[0];
        expect(foundItems).to.be.an("array").with.lengthOf(1);
        expect(foundItems[0].description).to.equal("Search desc");
    });

});
