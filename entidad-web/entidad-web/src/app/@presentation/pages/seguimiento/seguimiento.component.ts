import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';

@Component({
  selector: 'serv-talento-test',
  templateUrl: './seguimiento.component.html',
  styleUrls: ['./seguimiento.component.scss'],
})
export class SeguimientoComponent implements OnInit {
  filterForm: FormGroup;
  responsables = [];

  constructor(private fb: FormBuilder) { }

  ngOnInit(): void {
    this.initializeForm();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      codigo: '',
      nombrePerfil: '',
      rol: '',
      responsable: '',
      estado: '',
      fecha: '',
      regimen: '',
      modalidad: '',
      tipo: '',
      tipoPractica: '',
      condicion: '',
    });
  }

  clear() { }
  search() { }
}
