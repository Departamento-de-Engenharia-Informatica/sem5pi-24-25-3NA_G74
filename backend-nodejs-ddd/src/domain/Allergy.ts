import {AggregateRoot} from "../core/domain/AggregateRoot";
import {UniqueEntityID} from "../core/domain/UniqueEntityID";
import {Result} from "../core/logic/Result";
import {Guard} from "../core/logic/Guard";
import {ALL} from "node:dns";
import {IAllergyDTO} from "../dto/IAllergyDTO";

interface AllergyProps{
    code: string,
    designation: string,
    description: string
}

export class Allergy extends AggregateRoot<AllergyProps>{

    get id(): UniqueEntityID {
        return this._id;
    }

    get code(): string {
        return this.props.code;
    }

    set code(value: string) {
        this.props.code = value;
    }

    get designation(): string {
        return this.props.designation;
    }

    set designation(value: string) {
        this.props.designation = value;
    }

    get description(): string {
        return this.props.description;
    }

    set description(value: string) {
        this.props.description = value;
    }

    private constructor(props: AllergyProps, id?: UniqueEntityID) {

        if (!Allergy.validateAllergyCode(props.code)) {
            throw new Error("Invalid medical condition code");
        }
        if (!Allergy.validateDesignationLenght(props.designation)) {
            throw new Error("Invalid designation length");
        }
        if (!Allergy.validateDescriptionLenght(props.description)) {
            throw new Error("Invalid description length");
        }
        super(props, id);
    }

    private static validateAllergyCode(allergyCode: string): boolean {

        const SNOMED_CT_PATTERN = /^\d{6,18}$/;
        const ICD_11_PATTERN = /^[A-Z][0-9A-Z]{1,2}\.[0-9A-Z]{1,4}$/;

        if (!allergyCode.match(SNOMED_CT_PATTERN) && !allergyCode.match(ICD_11_PATTERN)) {
            return false;
        }

        return true;
    }

    private static validateDesignationLenght(designation: string): boolean {

        if (designation.length > 100) {
            return false;
        }

        return true;
    }
    private static validateDescriptionLenght(description: string): boolean {

        if (description.length > 2048) {
            return false;
        }

        return true;
    }
    
    public static create(allergyDTO: IAllergyDTO, id?: UniqueEntityID): Result<Allergy> {

        const guardedProps = [
            { argument: allergyDTO.code, argumentName: 'code' },
            { argument: allergyDTO.designation, argumentName: 'designation' },
            { argument: allergyDTO.description, argumentName: 'description' }
        ];

        const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

        if (!guardResult.succeeded) {
            return Result.fail<Allergy>(guardResult.message)
        } else {

            const allergy = new Allergy({
                code: allergyDTO.code,
                description: allergyDTO.description,
                designation: allergyDTO.designation,
            }, id);

            return Result.ok<Allergy>(allergy);

        }

    }
    
    
    
}