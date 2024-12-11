import {Allergy} from "../../../src/domain/Allergy";
import {UniqueEntityID} from "../../../src/core/domain/UniqueEntityID";

describe("Allergy Class", () => {
    it("should create an Allergy instance when valid properties are provided", () => {
        const props = {
            code: "A123",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).toBe(true);
        const allergy = result.getValue();

        expect(allergy).toBeInstanceOf(Allergy);
        expect(allergy.code).toBe("A123");
        expect(allergy.designation).toBe("Peanut Allergy");
        expect(allergy.description).toBe("An allergy to peanuts.");
    });

    it("should fail to create an Allergy instance if any property is missing", () => {
        const props = {
            code: null,
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toBe("code is null or undefined");
    });

    it("should fail to create an Allergy instance if all properties are missing", () => {
        const props = {
            code: null,
            designation: null,
            description: null,
        };

        const result = Allergy.create(props);

        expect(result.isSuccess).toBe(false);
        expect(result.error).toContain("code is null or undefined");
    });

    it("should allow updating the code property using its setter", () => {
        const props = {
            code: "A123",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.code = "B456";
        expect(allergy.code).toBe("B456");
    });

    it("should allow updating the designation property using its setter", () => {
        const props = {
            code: "A123",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.designation = "Dust Allergy";
        expect(allergy.designation).toBe("Dust Allergy");
    });

    it("should allow updating the description property using its setter", () => {
        const props = {
            code: "A123",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const result = Allergy.create(props);
        const allergy = result.getValue();

        allergy.description = "An allergy to dust.";
        expect(allergy.description).toBe("An allergy to dust.");
    });

    it("should create an Allergy instance with a specific UniqueEntityID", () => {
        const props = {
            code: "A123",
            designation: "Peanut Allergy",
            description: "An allergy to peanuts.",
        };

        const uniqueId = new UniqueEntityID("custom-id");
        const result = Allergy.create(props, uniqueId);

        expect(result.isSuccess).toBe(true);
        const allergy = result.getValue();

        expect(allergy.id).toBe(uniqueId);
    });
});
