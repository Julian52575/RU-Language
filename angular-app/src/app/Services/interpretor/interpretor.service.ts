import { Injectable } from '@angular/core';
import { lastValueFrom } from 'rxjs';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class InterpretorService {

  constructor(private http : HttpClient) { }

  public async runLispCode(code: string) : Promise<string> {
    const response = await lastValueFrom(this.http.post<any>('http://localhost:3000/lisp', { "code": code }));
    if (response) {
      return response.output;
    }
    return "";
  }

  public async runRuCode(code: string) : Promise<string> {
    const response = await lastValueFrom(this.http.post<any>('http://localhost:3000/ru', { "code": code }));
    if (response) {
      return response.output;
    }
    return "";
  }
}
