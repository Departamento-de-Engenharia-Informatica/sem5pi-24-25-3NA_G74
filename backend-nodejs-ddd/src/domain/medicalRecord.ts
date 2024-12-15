import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Allergy } from "./Allergy";
import { MedicalCondition } from "./medicalCondition";

interface MedicalRecordProps
{
    medicalRecordCode: string;
    allergies: Set<Allergy>;
    medicalCondition: Set<MedicalCondition>;
    designation: string;
}

export class MedicalRecord extends AggregateRoot<MedicalRecordProps>{

    get getId(): UniqueEntityID 
    {
        return this._id;
    }

    get getAllergies(): Set<Allergy> 
    {
        return this.props.allergies;
    }

    get getMedicalCondition(): Set<MedicalCondition>
    {
        return this.props.medicalCondition;
    }

    get getDesignation(): string 
    {
        return this.props.designation;
    }

    set setAllergies(value: Set<Allergy>)
    {
        this.props.allergies = value;
    }

    set setMedicalCondition(value: Set<MedicalCondition>)
    {
        this.props.medicalCondition = value;
    }

    set setDesignation(value: string)
    {
        this.props.designation = value;
    }

    private constructor(props:MedicalRecordProps, id?: UniqueEntityID){
        super(props,id);
    }

    
}