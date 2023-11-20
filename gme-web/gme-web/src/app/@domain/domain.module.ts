import {
  ModuleWithProviders,
  NgModule,
  Optional,
  SkipSelf,
} from '@angular/core';
import { CommonModule } from '@angular/common';
import {
  AdministratorService,
  AuthenticationService,
  ParameterService,
  SunatService,
} from '../@data/services';
import { throwIfAlreadyLoaded } from './module-import-guard';
import { AuthenticationRepository } from './repository/authentication.repository';
import { ParameterRepository } from './repository/parameter.repository';
import { SunatRepository } from './repository/sunat.repository';
import { AdministratorRepository } from './repository/administrator.repository';

import { ReniecRepository } from './repository/reniec.repository';
import { ReniecService } from '../@data/services/reniec.service';

import { PerfilesRepository } from './repository/perfiles.repository';
import { PerfilesService } from 'src/app/@data/services/perfiles.service';

import { BannerRepository } from './repository/banner.repository';
import { BannerService } from '../@data/services/banner.service';
import { MaestraParametroRepository } from './repository/maestra-parametro.repository';
import { MaestraParametroService } from '../@data/services/maestra-parametro.service';

import { ServidoresRepository } from './repository/servidores.repository';
import { ServidoresService } from 'src/app/@data/services/servidores.service';

import { OrganigramaRepository } from './repository/organigrama.repository';
import { OrganigramaService } from 'src/app/@data/services/organigrama.service';

import { OrganoRepository } from './repository/organo.repository';
import { OrganoService } from 'src/app/@data/services/organo.service';

import { UnidadOrganicaService } from 'src/app/@data/services/unidad-organica.service';
import { UnidadOrganicaRepository } from './repository/unidad-organica.repository';

import { PuestoRepository } from './repository/puesto.repository';
import { PuestoService } from '../@data/services/puesto.service';

import { GestoresOrhRepository } from './repository/gestores-orh.repository';
import { GestoresOrhService } from '../@data/services/gestores-orh.service';
import { SolicitudRepository } from './repository/solicitud.repository';
import { SolicitudService } from '../@data/services/solicitud.service';
import { SolicitudExternaRepository } from './repository/solicitud.externa.repository';
import { SolicitudExternaService } from '../@data/services/solicitud-externa.service';
import { EntidadRepository } from './repository/entidad.repository';
import { ConfiguracionRepository } from './repository/configuracion.repository';
import { ConfiguracionService } from '../@data/services/configuracion.service';

const DATA_SERVICES = [
  {
    provide: AuthenticationRepository,
    useClass: AuthenticationService,
  },
  { provide: ParameterRepository, useClass: ParameterService },

  { provide: SunatRepository, useClass: SunatService },

  { provide: EntidadRepository, useClass: SunatService },

  { provide: AdministratorRepository, useClass: AdministratorService },
  { provide: ReniecRepository, useClass: ReniecService },

  { provide: PerfilesRepository, useClass: PerfilesService },

  { provide: BannerRepository, useClass: BannerService },

  { provide: MaestraParametroRepository, useClass: MaestraParametroService},

  { provide: ServidoresRepository, useClass: ServidoresService },
  { provide: OrganigramaRepository, useClass: OrganigramaService },

  { provide: OrganoRepository, useClass: OrganoService },

  { provide: PuestoRepository, useClass: PuestoService },

  { provide: GestoresOrhRepository, useClass: GestoresOrhService },
  {
    provide: UnidadOrganicaRepository,
    useClass: UnidadOrganicaService,

  },
  { provide: SolicitudRepository, useClass: SolicitudService },
  { provide: SolicitudExternaRepository, useClass: SolicitudExternaService },
  { provide: ConfiguracionRepository, useClass: ConfiguracionService },

];

@NgModule({
  declarations: [],
  imports: [CommonModule],
})
export class DomainModule {
  constructor(@Optional() @SkipSelf() parentModule: DomainModule) {
    throwIfAlreadyLoaded(parentModule, 'CoreModule');
  }

  static forRoot(): ModuleWithProviders<any> {
    return {
      ngModule: DomainModule,
      providers: [...DATA_SERVICES],
    } as ModuleWithProviders<any>;
  }
}
