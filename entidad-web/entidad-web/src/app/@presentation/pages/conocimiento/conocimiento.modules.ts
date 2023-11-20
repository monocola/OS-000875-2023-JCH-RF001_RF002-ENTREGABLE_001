import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbOptionModule,
  NbSelectModule,
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { ReactiveFormsModule } from '@angular/forms';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { CommonComponentsModule } from '../../@common-components/common-components.module';
import { ConocimientoComponent } from './conocimiento.component';
import { ConocimientoRoutingModule } from './conocimiento-routing.module';

@NgModule({
  declarations: [ConocimientoComponent],
  imports: [
    CommonModule,
    ConocimientoRoutingModule,
    MatDividerModule,
    NbButtonModule,
    NbFormFieldModule,
    NbOptionModule,
    NbSelectModule,
    CommonComponentsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NgxTrimDirectiveModule,
  ],
})
export class ConocimientoModule {}
