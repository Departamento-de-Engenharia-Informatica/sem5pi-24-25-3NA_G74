import { TestBed } from '@angular/core/testing';

import { StaffViewModel } from './staff.viewmodel';

describe('StaffViewModel', () => {
  let viewModel: StaffViewModel;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    viewModel = TestBed.inject(StaffViewModel);
  });

  it('should be created', () => {
    expect(viewModel).toBeTruthy();
  });
});
