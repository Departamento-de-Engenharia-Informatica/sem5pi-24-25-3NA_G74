import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Guard } from "../core/logic/Guard";
import { Result } from "../core/logic/Result";
import { IMedicalConditionDTO } from "../dto/IMedicalConditionDTO";
import { Allergy } from "./Allergy";

interface MedicalConditionProps {
    medicalConditionCode: string;
    designation: string;
    description: string;
    commonSymptoms: string;
}

export class MedicalCondition extends AggregateRoot<MedicalConditionProps> {

    get Id(): UniqueEntityID {
        return this._id;
    }

    get description(): string {
        return this.props.description;
    }

    set description(value: string) {
        this.props.description = value;
    }

    get medicalConditionCode(): string {
        return this.props.medicalConditionCode;
    }

    set medicalConditionCode(value: string) {
        this.props.medicalConditionCode = value;
    }

    get designation(): string {
        return this.props.designation;
    }

    set designation(value: string) {
        this.props.designation = value;
    }

    get commonSymptoms(): string {
        return this.props.commonSymptoms;
    }

    set commonSymptoms(value: string) {
        this.props.commonSymptoms = value;
    }
    

    private constructor(props: MedicalConditionProps, id?: UniqueEntityID) {

        if (!MedicalCondition.validateMedicalConditionCode(props.medicalConditionCode)) {
            throw new Error("Invalid medical condition code");
        }
        if (!MedicalCondition.validateDesignationLenght(props.designation)) {
            throw new Error("Invalid designation length");
        }
        if (!MedicalCondition.validateDescriptionLenght(props.description)) {
            throw new Error("Invalid description length");
        }
        super(props, id);
    }

    private static validateMedicalConditionCode(medicalConditionCode: string): boolean {
        
        const SNOMED_CT_PATTERN = /^\d{6,18}$/; 
        const ICD_11_PATTERN = /^[A-Z][0-9A-Z]{1,2}\.[0-9A-Z]{1,4}$/; 

        if (!medicalConditionCode.match(SNOMED_CT_PATTERN) && !medicalConditionCode.match(ICD_11_PATTERN)) {
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


    public static create(medicalConditionDTO: IMedicalConditionDTO, id?: UniqueEntityID): Result<MedicalCondition> {

        const guardedProps = [
            { argument: medicalConditionDTO.medicalConditionCode, argumentName: 'medicalConditionCode' },
            { argument: medicalConditionDTO.description, argumentName: 'description' },
            { argument: medicalConditionDTO.designation, argumentName: 'designation' },
            { argument: medicalConditionDTO.commonSymptoms, argumentName: 'commonSymptoms' }
        ];

        const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

        if (!guardResult.succeeded) {
            return Result.fail<MedicalCondition>(guardResult.message)
        } else {

            const medicalCondition = new MedicalCondition({
                medicalConditionCode: medicalConditionDTO.medicalConditionCode,
                description: medicalConditionDTO.description,
                designation: medicalConditionDTO.designation,
                commonSymptoms: medicalConditionDTO.commonSymptoms
            }, id);

            return Result.ok<MedicalCondition>(medicalCondition);

        }

    }




}