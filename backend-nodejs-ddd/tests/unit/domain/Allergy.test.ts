import { expect } from "chai";
import { Allergy } from "../../../src/domain/Allergy";
import { UniqueEntityID } from "../../../src/core/domain/UniqueEntityID";

describe("Allergy Class", () => {
    it("should create an Allergy instance when valid properties are provided", () => {
        const props = {
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).to.be.true;
        const allergy = result.getValue();

        expect(allergy).to.be.instanceOf(Allergy);
        expect(allergy.code).to.equal("123456");
        expect(allergy.designation).to.equal("Peanut Allergy");
        expect(allergy.description).to.equal("An allergy to peanuts.");
    });

    it("should fail to create an Allergy instance if any property is missing", () => {
        const props = {
            code: null,
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).to.be.false;
        expect(result.error).to.equal("code is null or undefined");
    });

    it("should fail to create an Allergy instance if all properties are missing", () => {
        const props = {
            code: null,
            designation: null,
            description: null,
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).to.be.false;
        expect(result.error).to.contain("code is null or undefined");
    });

    it("should allow updating the code property using its setter", () => {
        const props = {
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.code = "123465";
        expect(allergy.code).to.equal("123465");
    });

    it("should allow updating the designation property using its setter", () => {
        const props = {
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.designation = "Dust Allergy";
        expect(allergy.designation).to.equal("Dust Allergy");
    });

    it("should allow updating the description property using its setter", () => {
        const props = {
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.description = "An allergy to dust.";
        expect(allergy.description).to.equal("An allergy to dust.");
    });

    it("should create an Allergy instance with a specific UniqueEntityID", () => {
        const props = {
            code: "123456",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const uniqueId = new UniqueEntityID("custom-id");
        const result = Allergy.create(props, uniqueId);

        expect(result.isSuccess).to.be.true;
        const allergy = result.getValue();

        expect(allergy.id).to.deep.equal(uniqueId);
    });
});
