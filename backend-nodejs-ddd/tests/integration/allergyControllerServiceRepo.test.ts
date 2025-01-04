import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";
import mongoose, { Model } from "mongoose";
import { MongoMemoryServer } from "mongodb-memory-server";

import AllergyRepo from "../../src/repos/allergyRepo";
import AllergyService from "../../src/services/allergyService";
import AllergyController from "../../src/controllers/allergyController";
import {IAllergyPersistence} from "../../src/dataschema/IAllergyPersistence";
import {IAllergyDTO} from "../../src/dto/IAllergyDTO";

describe("Full Chain Test - (Controller + Service + Repository + In-memory Mongo) WITHOUT starting Express", () => {
    let mongoServer: MongoMemoryServer;
    let allergyModel: Model<any>;
    let allergyRepo: AllergyRepo;
    let allergyService: AllergyService;
    let allergyController: AllergyController;

    let req: Partial<Request>;
    let res: SinonStubbedInstance<Response>;
    let next: sinon.SinonStub;

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

    const sampleDTO: IAllergyDTO = {
        code: "123456",
        description: "Some description",
        designation: "Some designation"
    };

    before(async () => {
        mongoServer = await MongoMemoryServer.create();
        const uri = mongoServer.getUri();

        await mongoose.connect(uri, { dbName: "testdb" });

        allergyModel = mongoose.model<IAllergyPersistence & mongoose.Document>("Allergy", AllergySchema);

        allergyRepo = new AllergyRepo(allergyModel, console);
        allergyService = new AllergyService(allergyRepo);
        allergyController = new AllergyController(allergyService);
    });

    after(async () => {
        await mongoose.disconnect();
        await mongoServer.stop();
    });

    beforeEach(() => {
        req = { body: {}, params: {}, query: {} } as Partial<Request>;
        res = {
            status: sinon.stub().returnsThis(),
            json: sinon.stub().returnsThis(),
            send: sinon.stub().returnsThis()
        } as unknown as SinonStubbedInstance<Response>;
        next = sinon.stub();
        
        sinon.stub(allergyController, "fail").callsFake((error: any) => {
            res.status(500).send(error instanceof Error ? error.message : String(error));
            return res;
        });
        sinon.stub(allergyController, "notFound").callsFake((message?: unknown) => {
            res.status(404).send(typeof message === "string" ? message : "Not found");
            return res as unknown as Response;
        });
    });

    afterEach(async () => {
        await allergyModel.deleteMany({});
        sinon.restore();
    });

    it("should create a new allergy and persist it in the in-memory DB", async () => {
        // Arrange
        req.body = sampleDTO;

        // Act
        await allergyController.createAllergy(
            req as Request,
            res as Response,
            next as NextFunction
        );

        // Assert - Check that the response was 201, contained the correct JSON
        expect(res.status.calledWith(201)).to.be.true;
        expect(res.json.called).to.be.true;
        const returnedData = res.json.getCall(0).args[0];
        expect(returnedData).to.include({
            code: "123456",
            description: "Some description",
            designation: "Some designation"
        });

        // Also check that the data was really persisted in the DB
        const doc = await allergyModel.findOne({ code: "123456" });
        expect(doc).to.exist;
        expect(doc.description).to.equal("Some description");
    });

    it("should update a allergy in the DB via the controller", async () => {
        await allergyModel.create({
            code: sampleDTO.code,
            description: sampleDTO.description,
            designation: sampleDTO.designation,
            domainId: "some-random-id"
        });
        
        req.params = { code: "123456" };
        req.body = { designation: "Updated Designation" };

        await allergyController.updateAllergy(
            req as Request,
            res as Response,
            next as NextFunction
        );

        // Expect 200 status and updated object in the response
        expect(res.status.calledWith(200)).to.be.true;
        const updatedData = res.json.getCall(0).args[0];
        expect(updatedData).to.include({
            code: "123456",
            designation: "Updated Designation"
        });

        // Verify in DB
        const doc = await allergyModel.findOne({ code: "123456" });
        expect(doc.designation).to.equal("Updated Designation");
    });

    

    it("should search and find a record by code", async () => {
        await allergyModel.create({
            code: "123488",
            description: "Search desc",
            designation: "Search design",
            domainId: "some-other-id"
        });
        
        req.query = { code: "123488" };
        await allergyController.searchAllergy(
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
