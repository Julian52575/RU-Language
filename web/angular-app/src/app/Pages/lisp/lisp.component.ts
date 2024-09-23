import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { InterpretorService } from '../../Services/interpretor/interpretor.service';
import { HttpClient, HttpClientModule } from '@angular/common/http';

@Component({
  selector: 'app-lisp',
  standalone: true,
  imports: [FormsModule, HttpClientModule],
  providers: [InterpretorService],
  templateUrl: './lisp.component.html',
  styleUrl: './lisp.component.scss'
})
export class LispComponent {
  textarea: string = '';
  output: string = '';

  constructor(private interpretorService: InterpretorService) { }

  public async run() {
    const res = await this.interpretorService.runLispCode(this.textarea);
    this.output = res;
  }
}
