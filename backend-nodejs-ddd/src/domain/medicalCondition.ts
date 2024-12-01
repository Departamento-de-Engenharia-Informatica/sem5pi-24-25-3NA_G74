import { AggregateRoot } from "../core/domain/AggregateRoot";
import { UniqueEntityID } from "../core/domain/UniqueEntityID";
import { Guard } from "../core/logic/Guard";
import { Result } from "../core/logic/Result";
import { IMedicalConditionDTO } from "../dto/IMedicalConditionDTO";

interface MedicalConditionProps {
    description: string;
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

    private constructor(props: MedicalConditionProps, id?: UniqueEntityID) {
        super(props, id);
    }

    public static create(medicalConditionDTO: IMedicalConditionDTO, id?: UniqueEntityID): Result<MedicalCondition> {

        const guardedProps = [
            { argument: medicalConditionDTO.description, argumentName: 'description' }
        ];

        const guardResult = Guard.againstNullOrUndefinedBulk(guardedProps);

        if (!guardResult.succeeded) {
            return Result.fail<MedicalCondition>(guardResult.message)
        } else {

            const medicalCondition = new MedicalCondition({
                ...medicalConditionDTO
            }, id);

            return Result.ok<MedicalCondition>(medicalCondition);

        }

    }




}