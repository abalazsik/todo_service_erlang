CREATE SEQUENCE todo_id_seq START 2;

create table todo (
    id bigint primary key default nextval('todo_id_seq'::regclass),
    creation_date timestamp DEFAULT now(),
    description text,
    done boolean DEFAULT false,
    size_ character varying(1) DEFAULT 'm'
);

ALTER TABLE todo ADD CONSTRAINT todo_size_type CHECK ((size_) in ('s', 'm', 'l', 'h'));