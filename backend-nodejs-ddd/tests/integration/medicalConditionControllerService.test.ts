import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";

import MedicalConditionController from "../../src/controllers/MedicalConditionController";
import MedicalConditionService from "../../src/services/MedicalConditionService";
import IMedicalConditionRepo from "../../src/services/IRepos/IMedicalConditionRepo";
import { IMedicalConditionDTO } from "../../src/dto/IMedicalConditionDTO";
import { Result } from "../../src/core/logic/Result";

// Mock repository for integration
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

describe("Integration - MedicalConditionController + MedicalConditionService", () => {
  let mockRepo: MockMedicalConditionRepo;
  let medicalConditionService: MedicalConditionService;
  let medicalConditionController: MedicalConditionController;

  let req: Partial<Request>;
  let res: SinonStubbedInstance<Response>;
  let next: sinon.SinonStub;

  // This is a sample valid DTO
  const validMedicalConditionDTO: IMedicalConditionDTO = {
    medicalConditionCode: "TESTCODE",
    description: "Test description",
    designation: "Test designation",
    commonSymptoms: "cough, fever"
  };

  beforeEach(() => {
    mockRepo = new MockMedicalConditionRepo();
    medicalConditionService = new MedicalConditionService(mockRepo as any);
    medicalConditionController = new MedicalConditionController(medicalConditionService);

    req = { body: {}, params: {}, query: {} } as Partial<Request>;
    res = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub().returnsThis(),
      send: sinon.stub().returnsThis()
    } as unknown as SinonStubbedInstance<Response>;
    next = sinon.stub();

    // Stub out BaseController methods fail(...) and notFound(...) to avoid real calls
    sinon.stub(medicalConditionController, "fail").callsFake((error: any) => {
      res.status(500).send(error instanceof Error ? error.message : String(error));
      return res;
    });

    sinon.stub(medicalConditionController, "notFound").callsFake((message?: string) => {
      res.status(404).send(message || "Not found");
      return res;
    });
  });

  afterEach(() => {
    sinon.restore();
  });

  describe("createMedicalCondition", () => {
    it("should return 201 and created medical condition if service succeeds", async () => {
      // Arrange
      req.body = validMedicalConditionDTO;

      // Stub service so that it returns success
      const successResult = Result.ok<IMedicalConditionDTO>(validMedicalConditionDTO);
      sinon.stub(medicalConditionService, "createMedicalCondition").resolves(successResult);

      // Act
      await medicalConditionController.createMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(201)).to.be.true;
      expect(res.json.calledWith(validMedicalConditionDTO)).to.be.true;
    });

    it("should call next(error) and return 500 if service throws error", async () => {
      // Arrange
      req.body = validMedicalConditionDTO;
      const error = new Error("Something unexpected");
      sinon.stub(medicalConditionService, "createMedicalCondition").rejects(error);

      // Act
      await medicalConditionController.createMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(500)).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("updateMedicalCondition", () => {
    it("should return 404 if service returns a failure result", async () => {
      // Arrange
      req.params = { medicalConditionCode: "TESTCODE" };
      sinon.stub(medicalConditionService, "UpdateMedicalCondition")
           .resolves(Result.fail<IMedicalConditionDTO>("Not found"));

      // Act
      await medicalConditionController.updateMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(404)).to.be.true;
    });

    it("should return 500 if service throws error", async () => {
      // Arrange
      req.params = { medicalConditionCode: "TESTCODE" };
      const error = new Error("Service exploded");
      sinon.stub(medicalConditionService, "UpdateMedicalCondition").rejects(error);

      // Act
      await medicalConditionController.updateMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(500)).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("searchMedicalCondition", () => {
    it("should return 404 if no medical conditions found (Result.fail)", async () => {
      // Arrange
      req.query = { medicalConditionCode: "NON_EXISTENT" };
      sinon.stub(medicalConditionService, "SearchMedicalCondition")
           .resolves(Result.fail("No medical conditions found"));

      // Act
      await medicalConditionController.searchMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(404)).to.be.true;
    });

    it("should return 200 and array of conditions on success", async () => {
      // Arrange
      req.query = { medicalConditionCode: "TESTCODE" };
      const successResult = Result.ok<IMedicalConditionDTO[]>([validMedicalConditionDTO]);
      sinon.stub(medicalConditionService, "SearchMedicalCondition").resolves(successResult);

      // Act
      await medicalConditionController.searchMedicalCondition(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(200)).to.be.true;
      expect(res.json.calledWith([validMedicalConditionDTO])).to.be.true;
    });
  });
});
