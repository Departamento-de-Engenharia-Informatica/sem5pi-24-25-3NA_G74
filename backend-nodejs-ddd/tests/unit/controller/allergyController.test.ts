import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";
import { Result } from "../../../src/core/logic/Result";
import AllergyController from "../../../src/controllers/allergyController";
import {IAllergyDTO} from "../../../src/dto/IAllergyDTO";

describe("AllergyController", () => {
  let allergyService: any;
  let allergyController: AllergyController;

  const mockAllergyDTO: IAllergyDTO = {
    code: "A12.34",
    description: "Valid description",
    designation: "Valid designation",
  };

  let req: Partial<Request>;
  let res: SinonStubbedInstance<Response>;
  let next: sinon.SinonStub;

  let failStub: sinon.SinonStub;
  let notFoundStub: sinon.SinonStub;

  beforeEach(() => {
    allergyService = {
      CreateAllergy: sinon.stub(),
      UpdateAllergy: sinon.stub(),
      SearchAllergy: sinon.stub(),
    };

    allergyController = new AllergyController(allergyService);

    // Mocking Express request/response
    req = { body: {}, params: {}, query: {} } as Partial<Request>;
    res = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub(),
      send: sinon.stub(),
    } as unknown as SinonStubbedInstance<Response>;
    next = sinon.stub();
    
    failStub = sinon.stub(allergyController, "fail").callsFake((error: any) => {
      res.status(500);
      res.send(error instanceof Error ? error.message : String(error));
      return res;
    });

    notFoundStub = sinon.stub(allergyController, "notFound").callsFake((message?: string) => {
      res.status(404);
      res.send(message || "Not found");
      return res;
    });
  });

  afterEach(() => {
    failStub.restore();
    notFoundStub.restore();
    sinon.restore();
  });

  describe("CreateAllergy", () => {
    it("should return 201 and the created allergy on success", async () => {
      req.body = mockAllergyDTO;
      allergyService.CreateAllergy.resolves(Result.ok(mockAllergyDTO));

      await allergyController.createAllergy(req as Request, res as Response, next as NextFunction);

      expect(res.status.calledWith(201)).to.be.true;
      expect(res.json.calledWith(mockAllergyDTO)).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.body = mockAllergyDTO;
      const error = new Error("Unexpected error");
      allergyService.CreateAllergy.rejects(error);

      await allergyController.createAllergy(req as Request, res as Response, next as NextFunction);
      
      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("UpdateAllergy", () => {
    it("should return 404 when allergy is not found", async () => {
      req.params = { code: "A12.34" };
      allergyService.UpdateAllergy.resolves(Result.fail("Allergy not found"));

      await allergyController.updateAllergy(req as Request, res as Response, next as NextFunction);
      
      expect(res.status.calledWith(404)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.params = { code: "A12.34" };
      const error = new Error("Unexpected error");
      allergyService.UpdateAllergy.rejects(error);

      await allergyController.updateAllergy(req as Request, res as Response, next as NextFunction);

      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("SearchAllergy", () => {
    it("should return 404 when no allergy is found", async () => {
      req.query = { code: "NON_EXISTENT_CODE" };
      allergyService.SearchAllergy.resolves(Result.fail("No allergy found"));

      await allergyController.searchAllergy(req as Request, res as Response, next as NextFunction);

      expect(res.status.calledWith(404)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.query = { code: "A12.34" };
      const error = new Error("Unexpected error");
      allergyService.SearchAllergy.rejects(error);

      await allergyController.searchAllergy(req as Request, res as Response, next as NextFunction);
      
      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });
});
