import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { ExtraRoutingModule } from './extra-routing.module';
import { ExtraComponent } from './extra.component';
import { RouterModule } from '@angular/router';
import { MatMenuModule } from '@angular/material/menu';
import { ThemeModule } from '../@theme/theme.module';
import { HeaderComponent } from './components/header/header.component';
import { SolicitudComponent } from './solicitud/solicitud.component';
import { SolicitudJefeOrhComponent } from './solicitud-jefe-orh/solicitud-jefe-orh.component';

import {
  NbActionsModule,
  NbAutocompleteModule,
  NbButtonModule, NbCardModule, NbDatepickerModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbLayoutModule, NbMenuModule,
  NbSelectModule, NbSidebarModule, NbStepperModule, NbTabsetModule
} from '@nebular/theme';
import { NgxDropzoneModule } from 'ngx-dropzone';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatDividerModule } from '@angular/material/divider';
import { MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { CommonComponentsModule } from '../@common-components/common-components.module';
import { SolicitudEditarComponent } from './solicitud-editar/solicitud-editar.component';

@NgModule({
  declarations: [
    ExtraComponent,
    HeaderComponent,
    SolicitudComponent,
    SolicitudJefeOrhComponent,
    SolicitudEditarComponent
  ],
  imports: [
    MatDialogModule,
    NbDatepickerModule,
    CommonModule,
    ExtraRoutingModule,
    RouterModule,
    ThemeModule,
    NbMenuModule,
    NbLayoutModule,
    NbSidebarModule,
    NbButtonModule,
    NbIconModule,
    MatMenuModule,
    FormsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    NbActionsModule,
    NgxDropzoneModule,
    NbAutocompleteModule,
    CommonComponentsModule,
    NbTabsetModule, NbCardModule, NbFormFieldModule, NbStepperModule,
  ],
  providers: [
    {
      provide: MatDialogRef,
      useValue: {}
    }
  ]
})
export class ExtraModule { }
