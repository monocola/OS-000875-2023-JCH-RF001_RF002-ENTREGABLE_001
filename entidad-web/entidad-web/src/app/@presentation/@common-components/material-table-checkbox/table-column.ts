export interface TableColumn {
  name: string;
  dataKey: string;
  position?: 'right' | 'left' | 'center';
  isSortable?: boolean;
  width?: string;
  descripcion: string;
  flagSeleccion?: boolean;
  preguntaId: number;
}
