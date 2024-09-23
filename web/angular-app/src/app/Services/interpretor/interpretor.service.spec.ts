import { TestBed } from '@angular/core/testing';

import { InterpretorService } from './interpretor.service';

describe('InterpretorService', () => {
  let service: InterpretorService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(InterpretorService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
