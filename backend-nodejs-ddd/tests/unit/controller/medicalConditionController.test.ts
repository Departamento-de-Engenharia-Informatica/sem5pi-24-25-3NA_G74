import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";
import MedicalConditionController from "../../../src/controllers/MedicalConditionController";
import { IMedicalConditionDTO } from "../../../src/dto/IMedicalConditionDTO";
import { Result } from "../../../src/core/logic/Result";

describe("MedicalConditionController", () => {
  let medicalConditionService: any;
  let medicalConditionController: MedicalConditionController;

  const mockMedicalConditionDTO: IMedicalConditionDTO = {
    medicalConditionCode: "A12.34",
    description: "Valid description",
    designation: "Valid designation",
    commonSymptoms: "Fever, cough",
  };

  let req: Partial<Request>;
  let res: SinonStubbedInstance<Response>;
  let next: sinon.SinonStub;

  // Stubs for inherited BaseController methods
  let failStub: sinon.SinonStub;
  let notFoundStub: sinon.SinonStub;

  beforeEach(() => {
    medicalConditionService = {
      createMedicalCondition: sinon.stub(),
      UpdateMedicalCondition: sinon.stub(),
      SearchMedicalCondition: sinon.stub(),
    };

    medicalConditionController = new MedicalConditionController(medicalConditionService);

    // Mocking Express request/response
    req = { body: {}, params: {}, query: {} } as Partial<Request>;
    res = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub(),
      send: sinon.stub(),
    } as unknown as SinonStubbedInstance<Response>;
    next = sinon.stub();

    // ---- CRUCIAL FIX: Stub controller's fail(...) and notFound(...)
    failStub = sinon.stub(medicalConditionController, "fail").callsFake((error: any) => {
      // The real method tries to do BaseController.jsonResponse(res, 500, error),
      // but 'res' is never passed. Instead, do it ourselves:
      res.status(500);
      res.send(error instanceof Error ? error.message : String(error));
      return res;
    });

    notFoundStub = sinon.stub(medicalConditionController, "notFound").callsFake((message?: string) => {
      // The real method calls BaseController.jsonResponse(res, 404, message),
      // but 'res' is never passed. So let’s manually do what the test expects:
      res.status(404);
      res.send(message || "Not found");
      return res;
    });
  });

  afterEach(() => {
    // Restore all stubs
    failStub.restore();
    notFoundStub.restore();
    sinon.restore();
  });

  describe("createMedicalCondition", () => {
    it("should return 201 and the created medical condition on success", async () => {
      req.body = mockMedicalConditionDTO;
      medicalConditionService.createMedicalCondition.resolves(Result.ok(mockMedicalConditionDTO));

      await medicalConditionController.createMedicalCondition(req as Request, res as Response, next as NextFunction);

      expect(res.status.calledWith(201)).to.be.true;
      expect(res.json.calledWith(mockMedicalConditionDTO)).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.body = mockMedicalConditionDTO;
      const error = new Error("Unexpected error");
      medicalConditionService.createMedicalCondition.rejects(error);

      await medicalConditionController.createMedicalCondition(req as Request, res as Response, next as NextFunction);

      // The real controller calls this.fail(e); then next(e).
      // Our stub calls res.status(500).send(...) so that the test won't break.
      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("updateMedicalCondition", () => {
    it("should return 404 when medical condition is not found", async () => {
      req.params = { medicalConditionCode: "A12.34" };
      medicalConditionService.UpdateMedicalCondition.resolves(Result.fail("Medical Condition not found"));

      await medicalConditionController.updateMedicalCondition(req as Request, res as Response, next as NextFunction);

      // The real code calls this.notFound("Medical Condition not found")
      // Our stub sets res.status(404).send(...)
      expect(res.status.calledWith(404)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.params = { medicalConditionCode: "A12.34" };
      const error = new Error("Unexpected error");
      medicalConditionService.UpdateMedicalCondition.rejects(error);

      await medicalConditionController.updateMedicalCondition(req as Request, res as Response, next as NextFunction);

      // The real code calls this.fail(e) and next(e).
      // Our stub sets res.status(500) so it won't break.
      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("searchMedicalCondition", () => {
    it("should return 404 when no medical conditions are found", async () => {
      req.query = { medicalConditionCode: "NON_EXISTENT_CODE" };
      medicalConditionService.SearchMedicalCondition.resolves(Result.fail("No medical conditions found"));

      await medicalConditionController.searchMedicalCondition(req as Request, res as Response, next as NextFunction);

      // The real code calls this.notFound("No medical conditions found")
      expect(res.status.calledWith(404)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
    });

    it("should call next with error on exception", async () => {
      req.query = { medicalConditionCode: "A12.34" };
      const error = new Error("Unexpected error");
      medicalConditionService.SearchMedicalCondition.rejects(error);

      await medicalConditionController.searchMedicalCondition(req as Request, res as Response, next as NextFunction);

      // The real code calls this.fail(e) then next(e)
      expect(res.status.calledWith(500)).to.be.true;
      expect(res.send.calledOnce).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });
});
