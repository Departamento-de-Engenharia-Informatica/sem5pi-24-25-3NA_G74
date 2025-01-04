import { expect } from "chai";
import sinon, { SinonStubbedInstance } from "sinon";
import { Request, Response, NextFunction } from "express";
import { Result } from "../../src/core/logic/Result";
import IAllergyRepo from "../../src/services/IRepos/IAllergyRepo";
import AllergyService from "../../src/services/allergyService";
import AllergyController from "../../src/controllers/allergyController";
import {IAllergyDTO} from "../../src/dto/IAllergyDTO";

// Mock repository for integration
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

describe("Integration - AllergyController + AllergyService", () => {
  let mockRepo: MockAllergyRepo;
  let allergyService: AllergyService;
  let allergyController: AllergyController;

  let req: Partial<Request>;
  let res: SinonStubbedInstance<Response>;
  let next: sinon.SinonStub;

  // This is a sample valid DTO
  const validAllergyDTO: IAllergyDTO = {
    code: "TESTCODE",
    description: "Test description",
    designation: "Test designation"
  };

  beforeEach(() => {
    mockRepo = new MockAllergyRepo();
    allergyService = new AllergyService(mockRepo as any);
    allergyController = new AllergyController(allergyService);

    req = { body: {}, params: {}, query: {} } as Partial<Request>;
    res = {
      status: sinon.stub().returnsThis(),
      json: sinon.stub().returnsThis(),
      send: sinon.stub().returnsThis()
    } as unknown as SinonStubbedInstance<Response>;
    next = sinon.stub();

    // Stub out BaseController methods fail(...) and notFound(...) to avoid real calls
    sinon.stub(allergyController, "fail").callsFake((error: any) => {
      res.status(500).send(error instanceof Error ? error.message : String(error));
      return res;
    });

    sinon.stub(allergyController, "notFound").callsFake((message?: string) => {
      res.status(404).send(message || "Not found");
      return res;
    });
  });

  afterEach(() => {
    sinon.restore();
  });

  describe("CreateAllergy", () => {
    it("should return 201 and created allergy if service succeeds", async () => {
      // Arrange
      req.body = validAllergyDTO;

      // Stub service so that it returns success
      const successResult = Result.ok<IAllergyDTO>(validAllergyDTO);
      sinon.stub(allergyService, "CreateAllergy").resolves(successResult);

      // Act
      await allergyController.createAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(201)).to.be.true;
      expect(res.json.calledWith(validAllergyDTO)).to.be.true;
    });

    it("should call next(error) and return 500 if service throws error", async () => {
      // Arrange
      req.body = validAllergyDTO;
      const error = new Error("Something unexpected");
      sinon.stub(allergyService, "CreateAllergy").rejects(error);

      // Act
      await allergyController.createAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(500)).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("updateAllergy", () => {
    it("should return 404 if service returns a failure result", async () => {
      // Arrange
      req.params = { code: "TESTCODE" };
      sinon.stub(allergyService, "UpdateAllergy")
           .resolves(Result.fail<IAllergyDTO>("Not found"));

      // Act
      await allergyController.updateAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(404)).to.be.true;
    });

    it("should return 500 if service throws error", async () => {
      // Arrange
      req.params = { code: "TESTCODE" };
      const error = new Error("Service exploded");
      sinon.stub(allergyService, "UpdateAllergy").rejects(error);

      // Act
      await allergyController.updateAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(500)).to.be.true;
      expect(next.calledWith(error)).to.be.true;
    });
  });

  describe("searchAllergy", () => {
    it("should return 404 if no allergies found (Result.fail)", async () => {
      // Arrange
      req.query = { code: "NON_EXISTENT" };
      sinon.stub(allergyService, "SearchAllergy")
           .resolves(Result.fail("No allergies found"));

      // Act
      await allergyController.searchAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(404)).to.be.true;
    });

    it("should return 200 and array of conditions on success", async () => {
      // Arrange
      req.query = { code: "TESTCODE" };
      const successResult = Result.ok<IAllergyDTO[]>([validAllergyDTO]);
      sinon.stub(allergyService, "SearchAllergy").resolves(successResult);

      // Act
      await allergyController.searchAllergy(
        req as Request,
        res as Response,
        next as NextFunction
      );

      // Assert
      expect(res.status.calledWith(200)).to.be.true;
      expect(res.json.calledWith([validAllergyDTO])).to.be.true;
    });
  });
});
