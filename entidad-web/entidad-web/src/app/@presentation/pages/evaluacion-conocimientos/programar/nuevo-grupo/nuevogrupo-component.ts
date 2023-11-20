import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';


@Component({
  selector: 'serv-talento-nuevogrupo',
  templateUrl: './nuevogrupo-component.html',
  styleUrls: ['./nuevogrupo-component.scss']
})

export class NuevoGrupoComponent implements OnInit {


  valForm: FormGroup;

  constructor(
    private fb: FormBuilder,
  ) { }


  ngOnInit(): void {
    this.initializeForm();
  }

  get f() {
    return this.valForm.controls;
  }

  initializeForm() {
    this.valForm = this.fb.group({
      convocatoria: '',
    });
  }

}
