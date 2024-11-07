import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ListAllOperationComponent } from './list-all-operation.component';

describe('ListAllOperationComponent', () => {
  let component: ListAllOperationComponent;
  let fixture: ComponentFixture<ListAllOperationComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [ListAllOperationComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(ListAllOperationComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
